(ns plauna.application
  (:require [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [clojure.core.async :as async]
            [plauna.core.email :as core-email]
            [plauna.util.page :as page]
            [plauna.database :as db])
  (:import (jakarta.mail Folder)))

(set! *warn-on-reflection* true)

(defn- filter->sql-clause [filter]
  (cond
    (= filter "enriched-only") {:where [:and [:<> :metadata.category nil] [:<> :metadata.language nil]] :order-by [[:date :desc]]}
    (= filter "without-category") {:where [:= :metadata.category nil] :order-by [[:date :desc]]}
    :else {:order-by [[:date :desc]]}))

(defn- search->sql-clause [search-text]
  (if (some? search-text)
    {:where [:or
             [:like :headers.subject (str "%" search-text "%")]
             [:like :bodies.content (str "%" search-text "%")]
             [:like :contacts.name (str "%" search-text "%")]
             [:like :contacts.address (str "%" search-text "%")]]
     :order-by [[:date :desc]]}
    {:order-by [[:date :desc]]}))

(defn- combine-maps-with [map1 map2 key combination-key]
  (let [val1 (get map1 key)
        val2 (get map2 key)]
    (cond (nil? val1) map2
          (nil? val2) map1
          :else (conj map1 {key [combination-key val1 val2]}))))

(defn- success-result [result-type data] (conj {:result result-type} data))

(defn- error-result [exception alert-content] {:result :error :exception exception :message {:type :alert :content alert-content}})

(defn categories
  "There is no entry for 'no entry' in the database. This function adds a 'n/a' entry to the actual list."
  [db] (conj (int/fetch-categories db) {:id -1 :name "n/a"}))

(defn connect-to-client
  "Returns {:result :ok} or {:result :redirect :provider provider} in case of oauth2"
  [connection {:keys [db] :as _}]
  (try
    (if (= "oauth2" (:auth-type (:config connection)))
      (let [auth-provider (int/fetch-auth-provider db (:auth-provider connection))
            oauth-data (int/fetch-oauth-token-data db (:id connection))]
        (cond
          (nil? auth-provider) (throw (ex-info "Auth type is 'oauth2' but there is no auth provider." {:connection connection}))
          (or (nil? oauth-data) (nil? (:access-token oauth-data)) (nil? (:refresh-token oauth-data)))
          (do
            (t/log! :warn ["Connection" (:user connection) (:host connection) "is set to use oauth2 but has no tokens in the db. You need to login manually from the 'Connections' page first."])
            (success-result :redirect {:provider (int/fetch-auth-provider db (:auth-provider (:config connection)))}))
          :else (do (int/connect connection)
                    (int/monitor-folders connection)
                    (success-result :ok nil))))
      (do (int/connect connection)
          (int/monitor-folders connection)
          {:result :ok}))
    (catch Exception e (do (t/log! :error ["There was an error when trying to log in:" e])
                           (error-result e "There was an error when trying to log in.")))))

(defn fetch-emails
  "Returns a list of emails. Customizable by parameters which can contain the following keys:
   :size, :page, :filter (all, enriched-only, or without-category), :search-field (subject), :search-text"
  [context parameters]
  (let [db (:db context)
        cat-list (categories db)
        customization-clause (combine-maps-with (filter->sql-clause (:filter parameters)) (search->sql-clause  (:search-text parameters)) :where :and)
        result (int/fetch-emails db {:entity :enriched-email :strict true :page (page/page-request (:page parameters) (:size parameters))} customization-clause)]
    {:data (:data result)
     :parameters {:filter (:filter parameters)
                  :total-pages (page/calculate-pages-total (:total result) (:size parameters))
                  :size (:size parameters)
                  :page (:page result)
                  :total (:total result)
                  :search-text (:search-text parameters)}
     :optional {:categories cat-list}}))

(defn- add-sanitized-text-to-enriched-email [context email]
  {:header (:header email)
   :metadata (:metadata email)
   :participants (:participants email)
   :body (mapv (fn [body-part] (if (core-email/body-text-content? body-part)
                                 (conj body-part {:sanitized-content (int/normalize (:analyzer context) body-part)})
                                 body-part)) (:body email))})

(defn fetch-email [context id]
  (let [db (:db context)
        cat-list (categories db)
        email (->> (int/fetch-emails db {:entity :enriched-email :strict false} {:where [:= :message-id id]})
                   first
                   (add-sanitized-text-to-enriched-email context))]
    {:data email
     :optional {:categories cat-list}}))

(defn create-new-category! [context category]
  (let [db (:db context)
        client (:client context)]
    (int/save-category db category)
    (doseq [connection-data (vals (int/connections client))]
      (int/create-category-directories! client connection-data [category]))))

(defn move-email-to-category
  [connection email category]
  (try
    (let [message-id (core-email/message-id email)
          old-category (core-email/category email)
          result (int/move-email-by-id connection message-id old-category category)]
      (if (true? result)
        (success-result :ok nil)
        (error-result nil "Moving email failed. Please check the logs.")))
    (catch Exception e (t/log! :error e) (error-result e "Moving email failed. Please check the logs."))))

(defn- move-message
  "category-object must contain category and category-id"
  [move? connection folder email message category-object]
  (if (and (true? move?) (some? category-object))
    (do (int/move-message connection message folder category-object)
        (t/log! :debug ["Email with subject:" (core-email/subject email) "was successfully moved to the corresponding folder"]))
    (do (t/log! :debug ["move option:" move? "category:" category-object "the email" (core-email/subject email) "will not be moved"])
        :na)))

(defn- incoming-email-workflow
  ([email connection]
   (let [{:keys [analyzer db]} (:context connection)
         enriched-email (int/enrich-email analyzer email)
         enriched-email-with-connection-id (assoc-in enriched-email [:metadata :connection-id] (:id connection))
         category (core-email/category enriched-email)]
     (t/log! :info ["Email with subject:" (core-email/subject email) "was categorized as" category])
     (int/save-email db enriched-email-with-connection-id)
     {:category category :category-id (core-email/category-id enriched-email)}))
  ([email message folder connection {:keys [move? assigned-category assigned-category-id]}]
   (let [{:keys [analyzer db]} (:context connection)]
     (if (not (empty? assigned-category))
       (let [language-result (int/detect-language analyzer email)
             enriched-email (core-email/construct-enriched-email email {:language (:code language-result) :language-confidence (:confidence language-result)} {:category assigned-category :category-id assigned-category-id :category-confidence 1} (:id connection))]
         (int/save-email db enriched-email)
         (t/log! :info ["Email with subject:" (core-email/subject email) "was successfully saved to the database"])
         (move-message move? connection folder email message assigned-category)
         {:category assigned-category})
       (let [enriched-email (int/enrich-email analyzer email)
             enriched-email-with-connection-id (assoc-in enriched-email [:metadata :connection-id] (:id connection))
             category (core-email/category enriched-email)]
         (int/save-email db enriched-email-with-connection-id)
         (t/log! :info ["Email with subject:" (core-email/subject email) "was successfully saved to the database"])
         (move-message move? connection folder email message {:category category :category-id (core-email/category-id enriched-email)})
         {:category category :category-id (core-email/category-id email)})))))

(defn handle-incoming-imap-email
  "Handle incoming emails synchronously on a single thread. Returns a result."
  [parsed-email connection]
  (try (let [result (incoming-email-workflow parsed-email connection)]
         (success-result :ok result))
       (catch Exception e (error-result e "Error encountered when processing incoming email"))))

(def ^:dynamic *retry-delays*
  "Backoff delays in milliseconds between attempts for transient errors during a parse run.
  Dynamic so tests can shrink it."
  [2000 10000 30000])

(defn- log-progress!
  "Log a one-line progress summary of a parse run."
  [folder-name processed total saved skipped]
  (t/log! :info ["Parsing" folder-name ":" processed "/" total
                 "messages processed (" saved "saved," skipped "skipped)"]))

(defn- poison-message?
  "True if the exception chain contains an IMAP parse/poison error (e.g. a message
  envelope that angus-mail refuses to parse). Such messages are skipped, not retried:
  retrying cannot help and would hang the run."
  [^Exception e]
  (loop [ex e]
    (cond
      (nil? ex) false
      (or (instance? org.eclipse.angus.mail.iap.ParsingException ex)
          (instance? jakarta.mail.internet.AddressException ex)
          (and (string? (.getMessage ex))
               (re-find #"(?i)failed to load imap envelope" (.getMessage ex))))
      true
      :else (recur (.getCause ex)))))

(defn- sleep-ms ^{:tag 'void} [^long ms]
  (try (Thread/sleep ms) (catch InterruptedException _)))

(defn- process-message-with-retries
  "Fetch and process the nth message of an already-open folder, retrying transient
  failures *retry-delays* times. Poison (unparseable) messages are skipped after a
  single attempt. Returns {:result :saved}, {:result :skipped, :exception e} or
  {:result :fatal, :exception e}."
  [connection ^Folder folder n options]
  (loop [attempt 0]
    (let [result (try
                   (let [email-message (int/message-at connection folder n)]
                     (incoming-email-workflow (:email email-message) (:message email-message) folder connection options)
                     {:result :saved})
                   (catch Exception e
                     (if (poison-message? e)
                       {:result :skipped :exception e}
                       {:result :transient :exception e})))]
      (case (:result result)
        :saved result
        :skipped result
        (:transient)
        (if (< attempt (count *retry-delays*))
          (do (t/log! :warn ["Transient error while processing message" n "in folder" (.getName folder)
                             "- attempt" (inc attempt) "of" (inc (count *retry-delays*)) ":"
                             (ex-message (:exception result))])
              (sleep-ms (nth *retry-delays* attempt))
              ;; a transient connection failure may have closed the folder under us
              (when-not (.isOpen folder)
                (t/log! :info ["Folder" (.getName folder) "was closed, re-opening it before retrying"])
                (.open folder Folder/READ_WRITE))
              (recur (inc attempt)))
          {:result :fatal :exception (:exception result)})))))

(defn- parse-folder-loop
  "Process messages n..1 of the open folder, descending so that expunges/moves only
  shift indices we have already passed. Returns the final run counters:
  {:status ... :done ... :saved ... :skipped ... :error ...}. Never throws."
  [connection ^Folder folder total options]
  (loop [n total
         saved 0
         skipped 0]
    (if (zero? n)
      {:status :completed :done total :saved saved :skipped skipped}
      (let [result (try
                     (process-message-with-retries connection folder n options)
                     (catch Exception e {:result :fatal :exception e}))
            outcome (:result result)]
        (case outcome
          :saved
          (do (when (zero? (rem n 25))
                (log-progress! (.getName folder) (inc (- total n)) total (inc saved) skipped))
              (recur (dec n) (inc saved) skipped))
          :skipped
          (let [ex (:exception result)]
            (t/log! {:level :error :error ex}
                    ["Skipping message" n "in folder" (.getName folder)
                     "because it could not be parsed:" (ex-message ex)])
            (when (zero? (rem n 25))
              (log-progress! (.getName folder) (inc (- total n)) total saved (inc skipped)))
            (recur (dec n) saved (inc skipped)))
          ;; :fatal — abort loudly with partial counts
          (let [ex (:exception result)
                msg (ex-message ex)]
            (t/log! {:level :error :error ex}
                    ["Aborting parse of" (.getName folder) "at message" n "of" total
                     "(" saved "saved," skipped "skipped so far):" msg])
            {:status :failed :done (- total n) :saved saved :skipped skipped :error msg}))))))

(defn read-emails-from-folder
  "Read all emails from a folder and process them. Returns the number of messages in
  the folder. Emails are processed asynchronously: the folder is opened once for the
  whole run and closed when the run ends. Poison (unparseable) messages are skipped
  and logged; transient errors are retried with backoff; a persistent failure fails
  the run loudly (logged) instead of dying silently."
  [^plauna.interfaces.IMAPConnection connection folder-name options]
  (let [messages-result (int/no-of-messages-in-folder connection folder-name)
        folder ^Folder (:folder messages-result)
        total (:message-count messages-result)]
    (if (> total 0)
      (do
        (t/log! :info ["There are" total "emails in" folder-name "The messages will get processed asynchronously"])
        (async/go
          (let [summary
                (try
                  (parse-folder-loop connection folder total options)
                  (catch Exception e
                    (t/log! {:level :error :error e} ["Unexpected error in parse run of" folder-name])
                    {:status :failed :error (ex-message e)})
                  (finally
                    (try (.close folder false)
                         (catch Exception e
                           (t/log! :warn ["Could not close folder" folder-name "after parse run:" (ex-message e)])))))]
            (t/log! :info ["Finished parsing" folder-name "-" (:status summary)
                           ":" (:saved summary) "saved," (:skipped summary) "skipped of" total
                           (when-let [err (:error summary)] (str " Error: " err))]))))
      (t/log! :info ["There are no emails in the folder. Doing nothing."]))
    total))

(defn recategorize-email [email category-id connection]
  (let [context (:context connection)
        db (:db context)
        message-id (core-email/message-id email)]
    (if-let [email-in-db (int/fetch-email db message-id)]
      (let [metadata (:metadata email-in-db)]
        (db/update-metadata
         message-id
         category-id
         1
         (:language metadata)
         (:language-confidence metadata)
         (:connection-id metadata)))
      (let [language-result (int/detect-language (:analyzer context) email)
            enriched-raw-email (-> email
                                   (assoc-in [:metadata :connection-id] (:id connection))
                                   (assoc-in [:metadata :language] (:code language-result))
                                   (assoc-in [:metadata :language-confidence] (:confidence language-result))
                                   (assoc-in [:metadata :category-id] category-id)
                                   (assoc-in [:metadata :category-confidence] 1))
            enriched-email (core-email/->EnrichedEmail
                            (:header enriched-raw-email)
                            (:body enriched-raw-email)
                            (:participants enriched-raw-email)
                            (:metadata enriched-raw-email))]
        (int/save-email (:db context) enriched-email)))))
