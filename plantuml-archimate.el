;;; plantuml-archimate.el --- Archimate helpers for PlantUML  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  shyam

;; Author: shyam
;; Keywords: convenience


;;; Commentary:

;;

;;; Code:

(defvar plantuml--archimate-relationships
  '("Composition"
    "Aggregation"
    "Assignment"
    "Specialization"
    "Serving"
    "Association"
    "Association_dir"
    "Flow"
    "Realization"
    "Triggering"
    "Access"
    "Access_r"
    "Access_rw"
    "Access_w"
    "Influence")
  "List of Archimate PlantUML relationships")

(defvar plantuml--archimate-elements
  '("Boundary"
    "Strategy_Resource"
    "Strategy_Capability"
    "Strategy_CourseOfAction"
    "Strategy_ValueStream"
    "Business_Actor"
    "Business_Role"
    "Business_Collaboration"
    "Business_Interface"
    "Business_Process"
    "Business_Function"
    "Business_Interaction"
    "Business_Event"
    "Business_Service"
    "Business_Object"
    "Business_Contract"
    "Business_Representation"
    "Business_Product"
    "Business_Location"
    "Application_Component"
    "Application_Collaboration"
    "Application_Interface"
    "Application_Function"
    "Application_Interaction"
    "Application_Process"
    "Application_Event"
    "Application_Service"
    "Application_DataObject"
    "Technology_Node"
    "Technology_Device"
    "Technology_SystemSoftware"
    "Technology_Collaboration"
    "Technology_Interface"
    "Technology_Path"
    "Technology_CommunicationNetwork"
    "Technology_Function"
    "Technology_Process"
    "Technology_Interaction"
    "Technology_Event"
    "Technology_Service"
    "Technology_Artifact"
    "Physical_Equipment"
    "Physical_Facility"
    "Physical_DistributionNetwork"
    "Physical_Material"
    "Motivation_Stakeholder"
    "Motivation_Driver"
    "Motivation_Assessment"
    "Motivation_Goal"
    "Motivation_Outcome"
    "Motivation_Principle"
    "Motivation_Requirement"
    "Motivation_Constraint"
    "Motivation_Meaning"
    "Motivation_Value"
    "Implementation_WorkPackage"
    "Implementation_Deliverable"
    "Implementation_Event"
    "Implementation_Plateau"
    "Implementation_Gap"
    "Other_Location"
    "Junction_Or"
    "Junction_And"
    "Grouping"
    "Group")
  "List of PlantUML Archimate elements")

(defvar plantuml--archimate-element-shortnames
  '(("collaboration" . "Coll")
    ("component" . "Comp")
    ("data" . "Data")
    ("event" . "Evt")
    ("function" . "Func")
    ("interaction" . "Intr")
    ("interface" . "Int")
    ("process" . "Proc")
    ("service" . "Svc")
    ("filled" . "Fld")
    ("unidirect" . "Unid")
    ("activity" . "Acti")
    ("actor" . "Act")
    ("contract" . "Ctrt")
    ("location" . "Loc")
    ("meaning" . "Mean")
    ("object" . "Obj")
    ("product" . "Prod")
    ("representation" . "Rep")
    ("role" . "Role")
    ("value" . "Value")
    ("path" . "Path")
    ("deliverable" . "Dlv")
    ("gap" . "Gap")
    ("plateau" . "Plat")
    ("workpackage" . "WP")
    ("required" . "Req")
    ("symmetric" . "Sym")
    ("assessment" . "Assmt")
    ("constraint" . "Con")
    ("driver" . "Drv")
    ("goal" . "Gaol")
    ("outcome" . "Outc")
    ("principle" . "Princ")
    ("requirement" . "Rqmt")
    ("stakeholder" . "Stkh")
    ("distribution" . "Dist")
    ("equipment" . "Eqmt")
    ("facility" . "Fac")
    ("material" . "Mat")
    ("capability" . "Cbp")
    ("course" . "Cour")
    ("resource" . "Rsrc")
    ("software" . "SW")
    ("artifact" . "Arti")
    ("Communication" . "Comm")
    ("device" . "Dev")
    ("infra" . "Infra")
    ("network" . "NW")
    ("node" . "Node")
    ("system" . "Sys"))
  "Shortnames for Archimate elements")

(defvar plantuml--archimate-category-shortnames
  '(("application" . "app")
    ("assessment" . "ass")
    ("association" . "asso")
    ("business" . "biz")
    ("communication" . "comm")
    ("constraint" . "con")
    ("deliverable" . "dlv")
    ("driver" . "drv")
    ("gap" . "gap")
    ("goal" . "goal")
    ("implementation" . "impl")
    ("interface" . "int")
    ("motivation" . "mot")
    ("physical" . "phy")
    ("principle" . "pri")
    ("requirement" . "req")
    ("stakeholder" . "sth")
    ("strategy" . "sty")
    ("system" . "sys")
    ("technology" . "tech")
    ("workpackage" . "wp"))
  "Shortnames for the Archimate categories")

(defvar plantuml--archimate-sprites
  '("application-collaboration"
    "application-component"
    "application-data-object"
    "application-event"
    "application-function"
    "application-interaction"
    "application-interface"
    "application-process"
    "application-service"
    "assessment-filled"
    "business-activity"
    "business-actor"
    "business-collaboration"
    "business-contract"
    "business-event"
    "business-function"
    "business-interaction"
    "business-interface"
    "business-location"
    "business-meaning"
    "business-object"
    "business-process"
    "business-product"
    "business-representation"
    "business-role"
    "business-service"
    "business-value"
    "communication-path"
    "constraint-filled"
    "deliverable-filled"
    "driver-filled"
    "gap-filled"
    "goal-filled"
    "implementation-deliverable"
    "implementation-event"
    "implementation-gap"
    "implementation-plateau"
    "implementation-workpackage"
    "interface-required"
    "interface-symmetric"
    "motivation-assessment"
    "motivation-constraint"
    "motivation-driver"
    "motivation-goal"
    "motivation-meaning"
    "motivation-outcome"
    "motivation-principle"
    "motivation-requirement"
    "motivation-stakeholder"
    "motivation-value"
    "physical-distribution-network"
    "physical-equipment"
    "physical-facility"
    "physical-material"
    "principle-filled"
    "requirement-filled"
    "stakeholder-filled"
    "strategy-capability"
    "strategy-course-of-action"
    "strategy-resource"
    "strategy-value-stream"
    "system-software"
    "technology-artifact"
    "technology-collaboration"
    "technology-communication-network"
    "technology-communication-path"
    "technology-device"
    "technology-event"
    "technology-function"
    "technology-infra-interface"
    "technology-infra-service"
    "technology-interaction"
    "technology-interface"
    "technology-network"
    "technology-node"
    "technology-path"
    "technology-process"
    "technology-service"
    "technology-system-software"
    "workpackage-filled")
  "List of Archimate sprites")

(defconst plantuml--archimate-include "!include <archimate/Archimate>"
  "Common include required for using Archimate components")

(defun plantuml--archimate-sprite (selection)
  (let ((sprite-name (cl-destructuring-bind
			 (category element) (split-string selection "-")
 		       (concat (cdr (assoc category  plantuml--archimate-category-shortnames))
 			       (cdr (assoc element  plantuml--archimate-element-shortnames))))))
    (format "sprite $%s jar:archimate/%s" sprite-name selection)))

(defun plantuml-insert-archimate-sprite ()
  (interactive)
  (when-let ((selection (completing-read
 			 "Select archimate artifact: "
			 plantuml--archimate-sprites)))
    (insert (plantuml--archimate-sprite selection))))

(defun plantuml--archimate-element (element name)
	(format "%s(%s, %S)" element (plantuml--make-alias name) name))

(defun plantuml-insert-archimate-element (element name)
  (interactive
   (list (completing-read
 	  "Archimate Element: " plantuml--archimate-elements nil t)
	 (read-string "Label/Name: ")))
  (when (and (> (length element) 0)
	     (> (length name) 0))
    (let ((common-include nil))
      (save-excursion
	(setq common-include (re-search-backward plantuml--archimate-include  nil t)))
      (save-excursion
	(when (re-search-backward "!include" nil t)
	  (goto-char (line-end-position))
	  (unless common-include
	    (insert "\n" plantuml--archimate-include)))))
    (insert (plantuml--archimate-element element name))))

(defun plantuml-archimate-convert-region (prefix start end)
  "Converts the selected region to a set of archimate element"
  (interactive "p\nr")
  (when (use-region-p)
    (let ((s (buffer-substring-no-properties start end)))
      (when-let (element (completing-read
			  "Archimate Element: "
			  plantuml--archimate-elements nil t))
	(atomic-change-group
	  (delete-region start end)
 	  (push-mark)
	  (dolist (name (split-string s "[\r\n]" t "[ \t]+"))
	    (insert (plantuml--archimate-element element name) "\n")))))))

(defun plantuml--find-archimate-elements ()
  (let ((els))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward
	      (rx-to-string `(: (group (or ,@plantuml--archimate-elements)) (* space)
				"(" (group (+ (not ","))) "," (* space)
				"\"" (group (+ (not "\""))) "\"" (* space)
				")"))
	      nil t)
	(push (list :element (match-string-no-properties 1)
 		    :alias (match-string-no-properties 2)
		    :label (match-string-no-properties 3))
	      els)))
    (reverse els)))

(defun plantuml--propertize-completion-list (pl)
  (cons
   (format
    "%s / %s" (plist-get pl :label)
    (propertize (plist-get pl :element)
		'face 'font-lock-type-face))
   pl))

(defun plantuml--completing-read (prompt els)
  (let ((pls (mapcar #'plantuml--propertize-completion-list els)))
    (when-let (selection (completing-read prompt pls))
      (cdr (assoc-string selection pls)))))

(defun plantuml--filter-selection (selection pls)
  (let ((alias (plist-get selection :alias)))
    (seq-filter
     (lambda (pl) (not (string= alias (plist-get pl :alias))))
     pls)))

(defun plantuml-insert-archimate-relationship ()
  (interactive)
  (let ((els (plantuml--find-archimate-elements))
	(src) (target) (relationship))
    (setq
     relationship (completing-read "Relationship: " plantuml--archimate-relationships)
     src (plantuml--completing-read "Source: " els)
     target (plantuml--completing-read "Target: " (plantuml--filter-selection src els)))
    (when (and (> (length relationship) 0)
	       (> (length src) 0)
	       (> (length target) 0))
      (insert (format "Rel_%s(%s, %s)" relationship
		      (plist-get src :alias)
		      (plist-get target :alias))))))

(keymap-set plantuml-add-map "a" #'plantuml-insert-archimate-sprite)
(keymap-set plantuml-mode-map "C-c I" #'plantuml-insert-archimate-element)
(keymap-set plantuml-mode-map "C-c l" #'plantuml-insert-archimate-relationship)
(keymap-set plantuml-mode-map "C-c R" #'plantuml-archimate-convert-region)

(provide 'plantuml-archimate)
;;; plantuml-archimate.el ends here
