(ns clox.scanner)

(def reserved-words
  #{"and" "class" "else" "false" "fun" "for"
    "if" "nil" "or" "print" "return" "super"
    "this" "true" "var" "while"})

(defn token-type-from-string [s]
  (keyword "clox.scanner" s))

(defn create-token
  ([type lexeme literal line]
   (cond-> {:type      type
            :lexeme    lexeme
            :line      line}
     literal (assoc :literal  literal))))

(defn create-error
  [msg lexeme line]
  {:message   msg
   :lexeme    lexeme
   :line      line
   :type :scanner})

(defn new-context [source]
  {:source source
   :tokens []
   :errors []
   :line 1
   :current 0
   :start 0})

(defn current-lexema [{:keys [start current source]}]
  (subs source start current))

(defn add-token
  ([ctx token-type] (add-token ctx token-type nil))
  ([{:keys [start current source line] :as ctx} token-type literal]
   (let [lexeme (current-lexema ctx)
         token (create-token token-type lexeme literal line)]
     (-> ctx
         (update :tokens conj token)))))

(defn add-error
  [{:keys [line] :as ctx} msg]
  (let [lexeme (current-lexema ctx)]
    (-> ctx
        (update :errors conj (create-error msg lexeme line)))))

(defn start-to-current [{current :current :as ctx}]
  (assoc ctx :start current))

(defn next-line [ctx]
  (update ctx :line inc))

(defn at-end? [{:keys [current source]}]
  (= current (count source)))

(defn advance [ctx]
  (update ctx :current inc))

(defn get-current [{:keys [source current] :as ctx}]
  (if (at-end? ctx)
    nil
    (nth source current)))

(defn get-next [ctx]
  (get-current (advance ctx)))


(defn match? [ctx to-char]
  (and
   (not (at-end? ctx))
   (= to-char (get-current ctx))))

(defn digit? [char]
  (and char
       (<= (int \0) (int char) (int \9))))

(defn alpha? [char]
  (and char
       (or (<= (int \a) (int char) (int \z))
           (<= (int \A) (int char) (int \Z)))))

(defn alpha-numeric? [char]
  (or (digit? char)
      (alpha? char)))

(defn match-string [ctx]
  (let [char (get-current ctx)]
    (case char
      nil      (add-error ctx "Unterminated string")
      \"       (-> ctx
                   advance
                   (add-token ::string (subs (current-lexema ctx) 1)))
      \newline (recur (-> ctx
                          advance
                          next-line))
      (recur (advance ctx)))))


(defn match-number [ctx]
  (let [char (get-current ctx)]
    (cond
      (digit? char) (recur (advance ctx))

      (and (= \. char)
           (digit? (get-next ctx)))
      (recur (advance ctx))

      :else (let [literal (-> ctx
                              current-lexema
                              Double/parseDouble)]
              (add-token ctx ::number literal)))))

(defn match-identifier [ctx]
  (let [char (get-current ctx)]
    (if (alpha-numeric? char)
      (recur (advance ctx))
      (let [literal (current-lexema ctx)]
        (add-token ctx (if (reserved-words literal)
                         (token-type-from-string literal)
                         ::identifier))))))

(defn scanToken [ctx]
  (let [char (get-current ctx)
        ctx (advance ctx)]
    (case char
      \( (add-token ctx ::lparen)
      \) (add-token ctx ::rparen)
      \{ (add-token ctx ::lbrace)
      \} (add-token ctx ::rbrace)
      \, (add-token ctx ::comma)
      \. (add-token ctx ::dot)
      \- (add-token ctx ::minus)
      \+ (add-token ctx ::plus)
      \; (add-token ctx ::semicolon)
      \* (add-token ctx ::star)

      \/ (if (match? ctx \/ )
           (loop [ctx' (advance ctx)]
             (prn ctx')
             (let [char (get-current ctx')]
               (case char
                 (\newline nil) ctx'
                 (recur (advance ctx')))))
           (add-token ctx ::slash))

      \! (if (match? ctx \= )
           (add-token (advance ctx) ::bang-equal)
           (add-token ctx ::bang))
      \= (if (match? ctx \= )
           (add-token (advance ctx) ::equal-equal)
           (add-token ctx ::equal))
      \> (if (match? ctx \= )
           (add-token (advance ctx) ::greater-equal)
           (add-token ctx ::greater))
      \< (if (match? ctx \= )
           (add-token (advance ctx) ::less-equal)
           (add-token ctx ::less))

      \" (match-string ctx)

      (\space \tab) ctx
      \newline (next-line ctx)

      (cond
        (digit? char) (match-number ctx)
        (alpha? char) (match-identifier ctx)
        :else (add-error ctx "Unexpected character.")))))

(defn scanner
  [source]
  (loop [ctx (new-context source)]
    (if (at-end? ctx)
      (add-token ctx ::eof)
      (recur (-> ctx
                 scanToken
                 start-to-current)))))

(comment
  (scanner "<=\n>= \"123\n45\" !")

  (scanner "<=\"12\n34\"\n!")

  (scanner ".\"1234")

  (scanner "!
<= // adsfdf
=")

  (scanner "443.44
<= // adsfdf
\"=\"")

  (scanner "dsaf adsf if (54)
dd ]]")

  (scanner "true")


  )
