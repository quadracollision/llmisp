(ns harness.gui-smoke)

(def sample-ast
  {:op "window"
   :title "Inventory Dashboard"
   :width 900
   :height 600
   :children [{:op "panel"
               :id "low-stock"
               :title "Low Stock"
               :layout "border"
               :children [{:op "label"
                           :id "low-stock-title"
                           :text "Low stock items require review."}
                          {:op "table"
                           :id "low-stock-table"
                           :columns ["sku" "warehouse" "on-hand" "reorder-action"]
                           :rows [{"sku" "A-100"
                                   "warehouse" "north"
                                   "on-hand" 4
                                   "reorder-action" "reorder"}]}
                          {:op "button"
                           :id "refresh"
                           :label "Refresh"
                           :action {:plugin "sqlite"
                                    :kind "sqlite-refresh-table"
                                    :db "gui-smoke.sqlite3"
                                    :table "submissions"
                                    :target "low-stock-table"
                                    :columns ["submitted-at" "sku" "status"]
                                    :message "Submissions refreshed."}}
                          {:op "text-field"
                           :id "sku"
                           :label "SKU"}
                          {:op "button"
                           :id "save"
                           :label "Save"
                           :action {:plugin "sqlite"
                                    :kind "sqlite-insert"
                                    :db "gui-smoke.sqlite3"
                                    :table "submissions"
                                    :values {"submitted-at" {:source "now"}
                                             "sku" {:source "control"
                                                    :id "sku"}
                                             "status" {:source "literal"
                                                       :value "draft"}}
                                    :message "Saved."}}
                          {:op "button"
                           :id "clear"
                           :label "Clear"
                           :action {:kind "clear-form"}}]}]})
