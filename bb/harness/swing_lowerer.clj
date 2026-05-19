(ns harness.swing-lowerer
  (:require [harness.action-plugins :as action-plugins]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn- layout-form [layout]
  (case (or layout "flow")
    "border" '(java.awt.BorderLayout.)
    "grid" '(java.awt.GridLayout. 0 1 8 8)
    "row" '(java.awt.FlowLayout. java.awt.FlowLayout/LEFT 8 4)
    '(java.awt.FlowLayout.)))

(declare node-form)

(defn- register-control-form [registry-sym node component-form]
  (if-let [id (:id node)]
    `(let [component# ~component-form]
       (swap! ~registry-sym assoc ~id component#)
       component#)
    component-form))

(defn- clear-form-action [registry-sym]
  `(doseq [component# (vals @~registry-sym)]
     (~'clear-control! component#)))

(defn- action-form [registry-sym action]
  (cond
    (:plugin action)
    (action-plugins/action-form {:registry-sym registry-sym} action)

    (= "show-message" (:kind action))
    `(javax.swing.JOptionPane/showMessageDialog nil ~(:message action))

    (= "clear-form" (:kind action))
    (clear-form-action registry-sym)

    :else
    nil))

(defn- button-form [registry-sym node]
  (let [button-form `(javax.swing.JButton. ~(:label node))]
    (if-let [action (:action node)]
      `(let [button# ~button-form]
         (.addActionListener
         button#
         (proxy [java.awt.event.ActionListener] []
            (~'actionPerformed [_#]
              ~(action-form registry-sym action))))
         button#)
      button-form)))

(defn- labeled-field-form [registry-sym node component-form]
  (if-let [label (:label node)]
    `(doto (javax.swing.JPanel. (java.awt.BorderLayout. 6 0))
       (.add (javax.swing.JLabel. ~label) java.awt.BorderLayout/WEST)
       (.add ~(register-control-form registry-sym node component-form) java.awt.BorderLayout/CENTER))
    (register-control-form registry-sym node component-form)))

(defn- table-data [node]
  (let [columns (:columns node)]
    (mapv (fn [row]
            (mapv #(get row %) columns))
          (or (:rows node) []))))

(def helper-forms
  [`(defn ~'control-value [component#]
      (cond
        (nil? component#) nil
        (instance? javax.swing.JTextField component#) (.getText component#)
        (instance? javax.swing.JTextArea component#) (.getText component#)
        (instance? javax.swing.JCheckBox component#) (.isSelected component#)
        (instance? javax.swing.JComboBox component#) (some-> component# .getSelectedItem str)
        (instance? javax.swing.JScrollPane component#) (~'control-value (.getView (.getViewport component#)))
        :else nil))
   `(defn ~'clear-control! [component#]
      (cond
        (nil? component#) nil
        (instance? javax.swing.JTextField component#) (.setText component# "")
        (instance? javax.swing.JTextArea component#) (.setText component# "")
        (instance? javax.swing.JCheckBox component#) (.setSelected component# false)
        (and (instance? javax.swing.JComboBox component#)
             (pos? (.getItemCount component#))) (.setSelectedIndex component# 0)
        (instance? javax.swing.JScrollPane component#) (~'clear-control! (.getView (.getViewport component#)))
        :else nil))
   `(defn ~'table-component [component#]
      (cond
        (instance? javax.swing.JTable component#) component#
        (instance? javax.swing.JScrollPane component#) (let [view# (.getView (.getViewport component#))]
                                                        (when (instance? javax.swing.JTable view#)
                                                          view#))
        :else nil))])

(defn- plugin-names [ast]
  (letfn [(walk [node]
            (concat (when-let [plugin (get-in node [:action :plugin])] [plugin])
                    (mapcat walk (:children node))
                    (mapcat (fn [tab] (mapcat walk (:children tab))) (:tabs node))))]
    (->> (walk ast) distinct vec)))

(defn- node-form [registry-sym node]
  (case (:op node)
    "window"
    (let [frame-sym (gensym "frame")
          root-sym (gensym "root")]
      `(let [~frame-sym (javax.swing.JFrame. ~(:title node))
             ~root-sym (javax.swing.JPanel. (java.awt.GridLayout. 0 1 8 8))]
         (.setDefaultCloseOperation ~frame-sym javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
         (.setSize ~frame-sym ~(:width node) ~(:height node))
         ~@(map (fn [child] `(.add ~root-sym ~(node-form registry-sym child)))
                (:children node))
         (.setContentPane ~frame-sym ~root-sym)
         ~frame-sym))

    "panel"
    (let [panel-sym (gensym "panel")]
      `(let [~panel-sym (javax.swing.JPanel. ~(layout-form (:layout node)))]
         ~@(when-let [title (:title node)]
             [`(.setBorder ~panel-sym (javax.swing.BorderFactory/createTitledBorder ~title))])
         ~@(map (fn [child] `(.add ~panel-sym ~(node-form registry-sym child)))
                (:children node))
         ~panel-sym))

    "label"
    `(javax.swing.JLabel. ~(:text node))

    "button"
    (register-control-form registry-sym node (button-form registry-sym node))

    "text-field"
    (labeled-field-form registry-sym node `(javax.swing.JTextField. ~(or (:columns node) 24)))

    "text-area"
    (labeled-field-form registry-sym node `(javax.swing.JScrollPane.
                                            (javax.swing.JTextArea. ~(or (:rows node) 4)
                                                                    ~(or (:columns node) 32))))

    "combo-box"
    (labeled-field-form registry-sym node `(javax.swing.JComboBox. (into-array String ~(vec (:options node)))))

    "check-box"
    (register-control-form registry-sym node `(javax.swing.JCheckBox. ~(:label node)))

    "table"
    (register-control-form
     registry-sym
     node
     `(javax.swing.JScrollPane.
       (javax.swing.JTable.
        (to-array-2d ~(table-data node))
        (into-array String ~(vec (:columns node))))))

    "tabs"
    `(let [tabs# (javax.swing.JTabbedPane.)]
       ~@(map (fn [tab]
                `(.addTab tabs# ~(:title tab)
                          ~(node-form registry-sym {:op "panel"
                                                    :id (:id tab)
                                                    :layout "flow"
                                                    :children (:children tab)})))
              (:tabs node))
       tabs#)))

(defn forms [ast]
  (let [registry-sym (gensym "registry")]
    (vec
     (concat
      [`(ns generated.swing-gui
          (:require [clojure.string])
          (:gen-class))]
      helper-forms
      (mapcat action-plugins/action-helper-forms (plugin-names ast))
      [`(defn ~'build-ui []
          (let [~registry-sym (atom {})]
            ~(node-form registry-sym ast)))
       `(defn ~'-main [& _args#]
          (javax.swing.SwingUtilities/invokeLater
           (fn []
             (.setVisible (~'build-ui) true))))]))))

(defn source [ast]
  (with-out-str
    (doseq [form (forms ast)]
      (pp/pprint form)
      (println))))

(defn write-source! [path ast]
  (spit path (source ast))
  path)
