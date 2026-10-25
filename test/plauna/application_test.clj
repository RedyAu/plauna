(ns plauna.application-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [clojure.java.io :as io]
            [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [plauna.core.email :refer :all]
            [plauna.database :as db]
            [plauna.analysis :an al]
            [plauna.imap.parser :as imap-parser]
            [plauna.files :as files]
            [plauna.application :as app])
  (:import [plauna.database SqliteDB]
           [plauna.analysis BasicAnalyzer]
           [jakarta.mail Folder MessagingException]))

(t/set-ns-filter! {:disallow "plauna.*"})

(t/set-min-level! :error)

(defn setup-clean-db [f]
  (swap! files/plauna-config (fn [_] {:data-folder "tmp/"}))
  (files/check-and-create-database-file)
  (db/create-db)
  (alter-var-root #'db/batch-size (fn [_] 2))
  (f)
  (files/delete-database-file))

(use-fixtures :each setup-clean-db)

(defn fake-connection-with-config [config]
  (defrecord TestConnection [config] int/IMAPConnection
             (connect [_])
             (monitor-folders [_]))
  (->TestConnection config))

(deftest basic-auth
  (let [database (reify int/DB (fetch-connection [_ id] {:id id :auth-type "basic"}))
        connection (reify int/IMAPConnection
                     (connect [_])
                     (monitor-folders [_]))
        context {:db database :client connection}]
    (is (= {:result :ok} (app/connect-to-client connection context))  "Basic authentication calls email-client's login method and returns ok")))

(deftest basic-auth-2
  (let [database (reify int/DB (fetch-connection [_ id] {:id id}))
        connection (reify int/IMAPConnection
                     (connect [_])
                     (monitor-folders [_]))
        context {:db database :client connection}]
    (is (= {:result :ok} (app/connect-to-client connection context))  "If no auth-type is defined, fall back on basic auth and return ok")))

(deftest oauth2-auth
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] {:id id}))
        connection (fake-connection-with-config {:auth-type "oauth2" :auth-provider 2})
        context {:db database :client connection}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client connection context))
        "auth-type 'oauth2' with auth provider but no token data returns a :redirect with the provider")))

(deftest oauth2-auth-2
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty" :refresh-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        connection (fake-connection-with-config {:auth-type "oauth2" :auth-provider 2})
        context {:db database :client connection}]
    (is (= {:result :ok}
           (app/connect-to-client connection context))
        "auth-type 'oauth2' with auth provider and token data calls client login and returns ok")))

(deftest oauth2-auth-3
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] nil))
        connection (fake-connection-with-config {:auth-type "oauth2" :auth-provider 2})
        context {:db database :client connection}]
    (is (= :error (:result (app/connect-to-client connection context))))
    "auth-type 'oauth2' with no auth provider returns an error"))

(deftest oauth2-auth-4
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        connection (fake-connection-with-config {:auth-type "oauth2" :auth-provider 2})
        context {:db database :client connection}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client connection context))
        "auth-type 'oauth2' with auth provider and access token but no refresh token calls client login and returns ok")))

(deftest emails-query-filter-wo-search
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:filter "enriched-only" :size 1})
    (is (= @query {:where [:and [:<> :metadata.category nil] [:<> :metadata.language nil]], :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:filter "without-category" :size 1})
    (is (= @query {:where [:= :metadata.category nil] :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:size 1})
    (is (= {:order-by [[:date :desc]]} @query))))

(deftest emails-query-search-wo-filter
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:search-field "subject" :search-text "test text" :size 1})
    (is (= {:where [:or [:like :headers.subject "%test text%"] [:like :bodies.content "%test text%"] [:like :contacts.name "%test text%"] [:like :contacts.address "%test text%"]], :order-by [[:date :desc]]} @query))))

(deftest emails-query-search-filter
  (let [query (atom "")
        database (reify int/DB
                   (fetch-categories [_] {})
                   (fetch-emails [_ _ important-query]
                     (swap! query (fn [_] important-query))
                     {:total 10 :size 1 :page 1}))]
    (app/fetch-emails {:db database} {:filter "enriched-only" :search-field "subject" :search-text "test text" :size 1})
    (is (= {:where [:and [:and [:<> :metadata.category nil] [:<> :metadata.language nil]] [:or [:like :headers.subject "%test text%"] [:like :bodies.content "%test text%"] [:like :contacts.name "%test text%"] [:like :contacts.address "%test text%"]]], :order-by [[:date :desc]]} @query))))

(deftest create-a-category
  (let [db-called (atom false)
        client-called (atom false)
        database (reify int/DB (save-category [_ _] (swap! db-called (fn [_] true))))
        client (reify int/EmailClient
                 (connections [_] {"does not matter" "some-data"})
                 (create-category-directories! [_ _ _] (swap! client-called (fn [_] true))))]
    (app/create-new-category! {:db database :client client} "test")
    (is (= true @db-called))
    (is (= true @client-called)))
  "Creating a new category makes correct database and client calls")

(def db-instance (SqliteDB.))

(def al-instance (BasicAnalyzer.))

(deftest recategorize-new-email
  (let [fake-conn {:id "test-conn-id" :context {:db db-instance :analyzer al-instance}}
        test-email (construct-enriched-email
                    (construct-email {:message-id "test" :date 0 :subject "Test" :in-reply-to nil :mime-type "text/plain"}
                                     [{:message-id "test" :mime-type "text/plain" :charset "fake" :transfer-encoding "fake" :content "Test" :sanitized-content "Test"}]
                                     [{:type :sender :message-id "test" :name "fake" :address "fake" :contact-key "fake"} {:type :receiver :message-id "test" :name "fake" :address "fake" :contact-key "fake"}])
                    nil nil "test-conn-id")]
    (.save-category db-instance "test-cat")
    (.save-category db-instance "another-cat") ;; assuming this one has id 2
    (testing "When there is no such email in the db"
      (app/recategorize-email test-email 1 fake-conn)
      (let [email-in-db (.fetch-email db-instance "test")
            metadata (:metadata email-in-db)]
        (is (= "test-conn-id" (:connection-id metadata)))
        (is (= "test-cat" (:category metadata)))
        (is (= 1.0 (:category-confidence metadata)))))
    (testing "When when email already exists in the db"
      (app/recategorize-email test-email 2 fake-conn)
      (let [email-in-db (.fetch-email db-instance "test")
            metadata (:metadata email-in-db)]
        (is (= "test-conn-id" (:connection-id metadata)))
        (is (= "another-cat" (:category metadata)))
        (is (= 1.0 (:category-confidence metadata)))))))

;; ---------- Parse run (read-emails-from-folder) tests ----------

(defn wait-for
  "Poll an atom until (pred @atom) is truthy or timeout-ms elapses."
  [atom-val pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [v @atom-val]
        (cond
          (pred v) v
          (< (System/currentTimeMillis) deadline) (do (Thread/sleep 20) (recur))
          :else ::timeout)))))

(defn fake-folder
  "Proxy jakarta.mail.Folder with a fixed message count. Tracks .close calls."
  [total close-atom]
  (let [dummy-store (.getStore (jakarta.mail.Session/getInstance (java.util.Properties.)) "imap")]
    (proxy [jakarta.mail.Folder] [dummy-store]
      (getName [] "TESTFOLDER")
      (isOpen [] true)
      (close [_] (when close-atom (swap! close-atom inc)))
      (getMessageCount [] (or total 0)))))

(defrecord TestParseConnection [id context folder open-count]
  int/IMAPConnection
  (config [_] {:imap {:id id}})
  (connect [_])
  (connected? [_] true)
  (disconnect-and-stop-monitoring [_])
  (list-folders [_])
  (no-of-messages-in-folder [_ _folder-name]
    (swap! open-count inc)
    {:message-count (.getMessageCount ^jakarta.mail.Folder folder) :connection-id id :folder folder})
  (message-at [_ _folder n] ((:message-at-fn context) folder n))
  (nth-message-in-folder [_ _folder-name _n] {:email nil :message (Object.)})
  (move-message [_ _m _s _t])
  (move-email-by-id [_ _m _s _t])
  (monitor-folders [_])
  (update-config [_ _c]))

(defn fake-analyzer []
  (reify int/Analyzer
    (enrich-email [_ email] email)
    (detect-language [_ _email] {:code "en" :confidence 0.9})
    (normalize [_ _body-part] nil)))

(defn fake-db [saved-atom]
  (reify int/DB
    (save-email [_ email] (swap! saved-atom conj email))))

(defn parse-conn-fn
  "Mock connection for parse tests.
  opts: :id :total :message-at-fn (fn [folder n] -> {:email ... :message ...}, may throw)
  Returns {:conn ... :open-count ... :close-count ... :saved ...}."
  [{:keys [id total message-at-fn]}]
  (let [close-atom (atom 0)
        open-count (atom 0)
        saved-atom (atom [])
        folder (fake-folder total close-atom)
        conn (->TestParseConnection id
                                    {:analyzer (fake-analyzer)
                                     :db (fake-db saved-atom)
                                     :message-at-fn (or message-at-fn (fn [_ _n] {:email {:header {:subject "s"} :metadata nil} :message (Object.)}))}
                                    folder
                                    open-count)]
    {:conn conn :folder folder :open-count open-count :close-count close-atom :saved saved-atom}))

(defn poison-exception
  "MessagingException caused by angus ParsingException caused by AddressException."
  []
  (MessagingException. "Failed to load IMAP envelope"
                       (org.eclipse.angus.mail.iap.ParsingException.
                        "ADDRESS parse error"
                        (jakarta.mail.internet.AddressException. "Domain contains dot-dot"))))

(def parse-options {:move? false :assigned-category nil :assigned-category-id nil})

(deftest parse-run-opens-folder-once-and-closes-it
  (let [{:keys [conn open-count close-count saved]} (parse-conn-fn {:id "conn-once" :total 3})
        total (app/read-emails-from-folder conn "TESTFOLDER" parse-options)]
    (is (= 3 total) "returns the number of messages in the folder")
    (let [v (wait-for close-count #(= 1 %) 5000)]
      (is (not= ::timeout v) "run should complete (folder closed)")
      (is (= 3 (count @saved)) "all messages processed and saved")
      (is (= 1 @open-count) "folder opened exactly once for the whole run")
      (is (= 1 @close-count) "folder closed exactly once after the run"))))

(deftest parse-run-skips-poison-messages
  (let [processed (atom [])
        {:keys [conn saved]}
        (parse-conn-fn {:id "conn-poison" :total 3
                        :message-at-fn (fn [_ n]
                                         (if (= n 2)
                                           (throw (poison-exception))
                                           (do (swap! processed conj n)
                                               {:email {:header {:subject "s"} :metadata nil} :message (Object.)})))})]
    (app/read-emails-from-folder conn "TESTFOLDER" parse-options)
    (let [v (wait-for saved #(= 2 (count %)) 5000)]
      (is (not= ::timeout v) "run completes despite the poison message")
      (is (= [3 1] @processed) "messages after the poison are still processed")
      (is (= 2 (count @saved)) "the two non-poison messages were saved"))))

(deftest parse-run-retries-transient-then-fails-loudly
  (let [attempts (atom 0)
        {:keys [conn saved]}
        (parse-conn-fn {:id "conn-fatal" :total 3
                        :message-at-fn (fn [_ _n]
                                         (swap! attempts inc)
                                         (throw (MessagingException. "connection failure")))})]
    (binding [app/*retry-delays* [50 50]]
      (app/read-emails-from-folder conn "TESTFOLDER" parse-options)
      (let [v (wait-for attempts #(= 3 %) 10000)]
        (is (not= ::timeout v) "run reaches a terminal state after exhausting retries")
        (is (= 3 @attempts) "1 initial + 2 retries on message 3 before failing the run")
        (is (= 0 (count @saved)) "nothing saved before the failure at message 3")))))
