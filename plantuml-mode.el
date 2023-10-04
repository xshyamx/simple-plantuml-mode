;;; Customizable variables
(defgroup plantuml-mode nil
  "Major mode for plantuml" :group
  'languages)

(defcustom plantuml-java-cm "java"
  "Path to java executable"
  :type 'string
  :group 'plantuml)

(defcustom plantuml-jar-path "plantuml.jar"
  "Path to plantuml.jar file"
  :type 'string
  :group 'plantuml)

;;; default mode map
(defvar plantuml-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'plantuml-preview)
    (define-key keymap (kbd "C-c C-o") #'plantuml-open-preview)
    (define-key keymap (kbd "C-c !") #'plantuml-select-diagram)
    keymap))
(defvar plantuml-mode-hook nil "Standard mode hook for plantuml-mode")

;;; syntax table
(defvar plantuml-mode-syntax-table
  (make-syntax-table)
  "Syntax table for plantuml-mode")

(modify-syntax-entry ?\' "<" plantuml-mode-syntax-table)
(modify-syntax-entry ?\n ">" plantuml-mode-syntax-table)

;;; constants
;; diagram types
(defconst plantuml-diagram-types
  '(("u" "UML Diagram" "uml")
    ("m" "Mindmap" "mindmap")
    ("s" "Salt UI wireframe" "salt")
    ("g" "Gantt chart" "gantt")
    ("w" "Work breakdown structure" "wbs")
    ("j" "JSON" "json")
    ("y" "YAML" "yaml"))
  "Types of PlantUML diagrams")

;; color codes extracted from https://www.w3.org/wiki/CSS/Properties/color/keywords
(defconst plantuml--colors
  '(("aliceblue" . "#f0f8ff")
    ("antiquewhite" . "#faebd7")
    ("aqua" . "#00ffff")
    ("aquamarine" . "#7fffd4")
    ("azure" . "#f0ffff")
    ("beige" . "#f5f5dc")
    ("bisque" . "#ffe4c4")
    ("black" . "#000000")
    ("blanchedalmond" . "#ffebcd")
    ("blue" . "#0000ff")
    ("blueviolet" . "#8a2be2")
    ("brown" . "#a52a2a")
    ("burlywood" . "#deb887")
    ("cadetblue" . "#5f9ea0")
    ("chartreuse" . "#7fff00")
    ("chocolate" . "#d2691e")
    ("coral" . "#ff7f50")
    ("cornflowerblue" . "#6495ed")
    ("cornsilk" . "#fff8dc")
    ("crimson" . "#dc143c")
    ("cyan" . "#00ffff")
    ("darkblue" . "#00008b")
    ("darkcyan" . "#008b8b")
    ("darkgoldenrod" . "#b8860b")
    ("darkgray" . "#a9a9a9")
    ("darkgreen" . "#006400")
    ("darkgrey" . "#a9a9a9")
    ("darkkhaki" . "#bdb76b")
    ("darkmagenta" . "#8b008b")
    ("darkolivegreen" . "#556b2f")
    ("darkorange" . "#ff8c00")
    ("darkorchid" . "#9932cc")
    ("darkred" . "#8b0000")
    ("darksalmon" . "#e9967a")
    ("darkseagreen" . "#8fbc8f")
    ("darkslateblue" . "#483d8b")
    ("darkslategray" . "#2f4f4f")
    ("darkslategrey" . "#2f4f4f")
    ("darkturquoise" . "#00ced1")
    ("darkviolet" . "#9400d3")
    ("deeppink" . "#ff1493")
    ("deepskyblue" . "#00bfff")
    ("dimgray" . "#696969")
    ("dimgrey" . "#696969")
    ("dodgerblue" . "#1e90ff")
    ("firebrick" . "#b22222")
    ("floralwhite" . "#fffaf0")
    ("forestgreen" . "#228b22")
    ("fuchsia" . "#ff00ff")
    ("gainsboro" . "#dcdcdc")
    ("ghostwhite" . "#f8f8ff")
    ("gold" . "#ffd700")
    ("goldenrod" . "#daa520")
    ("gray" . "#808080")
    ("green" . "#008000")
    ("greenyellow" . "#adff2f")
    ("grey" . "#808080")
    ("honeydew" . "#f0fff0")
    ("hotpink" . "#ff69b4")
    ("indianred" . "#cd5c5c")
    ("indigo" . "#4b0082")
    ("ivory" . "#fffff0")
    ("khaki" . "#f0e68c")
    ("lavender" . "#e6e6fa")
    ("lavenderblush" . "#fff0f5")
    ("lawngreen" . "#7cfc00")
    ("lemonchiffon" . "#fffacd")
    ("lightblue" . "#add8e6")
    ("lightcoral" . "#f08080")
    ("lightcyan" . "#e0ffff")
    ("lightgoldenrodyellow" . "#fafad2")
    ("lightgray" . "#d3d3d3")
    ("lightgreen" . "#90ee90")
    ("lightgrey" . "#d3d3d3")
    ("lightpink" . "#ffb6c1")
    ("lightsalmon" . "#ffa07a")
    ("lightseagreen" . "#20b2aa")
    ("lightskyblue" . "#87cefa")
    ("lightslategray" . "#778899")
    ("lightslategrey" . "#778899")
    ("lightsteelblue" . "#b0c4de")
    ("lightyellow" . "#ffffe0")
    ("lime" . "#00ff00")
    ("limegreen" . "#32cd32")
    ("linen" . "#faf0e6")
    ("magenta" . "#ff00ff")
    ("maroon" . "#800000")
    ("mediumaquamarine" . "#66cdaa")
    ("mediumblue" . "#0000cd")
    ("mediumorchid" . "#ba55d3")
    ("mediumpurple" . "#9370db")
    ("mediumseagreen" . "#3cb371")
    ("mediumslateblue" . "#7b68ee")
    ("mediumspringgreen" . "#00fa9a")
    ("mediumturquoise" . "#48d1cc")
    ("mediumvioletred" . "#c71585")
    ("midnightblue" . "#191970")
    ("mintcream" . "#f5fffa")
    ("mistyrose" . "#ffe4e1")
    ("moccasin" . "#ffe4b5")
    ("navajowhite" . "#ffdead")
    ("navy" . "#000080")
    ("oldlace" . "#fdf5e6")
    ("olive" . "#808000")
    ("olivedrab" . "#6b8e23")
    ("orange" . "#ffa500")
    ("orangered" . "#ff4500")
    ("orchid" . "#da70d6")
    ("palegoldenrod" . "#eee8aa")
    ("palegreen" . "#98fb98")
    ("paleturquoise" . "#afeeee")
    ("palevioletred" . "#db7093")
    ("papayawhip" . "#ffefd5")
    ("peachpuff" . "#ffdab9")
    ("peru" . "#cd853f")
    ("pink" . "#ffc0cb")
    ("plum" . "#dda0dd")
    ("powderblue" . "#b0e0e6")
    ("purple" . "#800080")
    ("red" . "#ff0000")
    ("rosybrown" . "#bc8f8f")
    ("royalblue" . "#4169e1")
    ("saddlebrown" . "#8b4513")
    ("salmon" . "#fa8072")
    ("sandybrown" . "#f4a460")
    ("seagreen" . "#2e8b57")
    ("seashell" . "#fff5ee")
    ("sienna" . "#a0522d")
    ("silver" . "#c0c0c0")
    ("skyblue" . "#87ceeb")
    ("slateblue" . "#6a5acd")
    ("slategray" . "#708090")
    ("slategrey" . "#708090")
    ("snow" . "#fffafa")
    ("springgreen" . "#00ff7f")
    ("steelblue" . "#4682b4")
    ("tan" . "#d2b48c")
    ("teal" . "#008080")
    ("thistle" . "#d8bfd8")
    ("tomato" . "#ff6347")
    ("turquoise" . "#40e0d0")
    ("violet" . "#ee82ee")
    ("wheat" . "#f5deb3")
    ("white" . "#ffffff")
    ("whitesmoke" . "#f5f5f5")
    ("yellow" . "#ffff00")
    ("yellowgreen" . "#9acd32"))
  "Platnuml predefined colors")


(defconst plantuml--color-names
  (mapcar #'car plantuml--colors)
  "Plantuml color names only")

(defun plantuml--find-color (color)
  "Find hex color code for named color (strips prefix # if present) or if in the form of #ffffff return that"
  (if (string-match-p "#[[:digit:]]\\{6\\}" color)
      color
    (let ((name (replace-regexp-in-string "^#" "" color)))
      (cdr (assoc name plantuml--colors)))))

;; most of the following constants are extracted from output of
;; 'java -jar plantuml.jar -language'
(defconst plantuml--component-types
  '("abstract" "actor" "agent" "annotation" "archimate" "artifact"
    "boundary" "card" "class" "cloud" "collections" "component" "control"
    "database" "diamond" "entity" "enum" "file" "folder" "frame" "hexagon"
    "interface" "label" "node" "object" "package" "participant" "person"
    "queue" "rectangle" "stack" "state" "storage"
    "usecase" )
  "Plantuml component types"
  )

(defconst plantuml--diagram-types
  '("board" "bpm" "creole" "cute" "def" "ditaa" "dot" "flow" "gantt"
    "git" "jockit" "json" "latex" "math" "mindmap" "nwdiag" "project"
    "salt" "tree" "uml" "wbs" "wire" "yaml")
  "Plantuml diagram types")

(defconst plantuml--keywords
  '("across" "activate" "again" "allow mixing" "allowmixing" "also"
    "alt" "as" "autonumber" "bold" "bottom" "box" "break" "caption"
    "center" "circle" "color" "create" "critical" "dashed"
    "deactivate" "description" "destroy" "detach" "dotted" "down"
    "else" "elseif" "empty" "end" "endif" "endwhile" "false" "footbox"
    "footer" "fork" "group"
    "header" "hide" "hnote" "if" "is" "italic"
    "kill" "left" "left to right direction" "legend" "link" "loop"
    "mainframe" "map" "members" "namespace" "newpage" "normal" "note"
    "of" "on" "opt" "order" "over" "package" "page" "par" "partition"
    "plain" "ref" "repeat" "return" "right" "rnote" "rotate" "show"
    "skin" "skinparam" "split" "sprite" "start" "stereotype" "stop"
    "style" "then" "title" "top" "top to bottom direction" "true" "up"
    "while")
  "Plantuml keywords")

(defconst plantuml--skin-parameters
  '("ActivityBackgroundColor" "ActivityBorderColor"
    "ActivityBorderThickness" "ActivityDiamondFontColor"
    "ActivityDiamondFontName" "ActivityDiamondFontSize"
    "ActivityDiamondFontStyle" "ActivityFontColor" "ActivityFontName"
    "ActivityFontSize" "ActivityFontStyle" "ActorBackgroundColor"
    "ActorBorderColor" "ActorFontColor" "ActorFontName" "ActorFontSize"
    "ActorFontStyle" "ActorStereotypeFontColor"
    "ActorStereotypeFontName" "ActorStereotypeFontSize"
    "ActorStereotypeFontStyle" "AgentBorderThickness" "AgentFontColor"
    "AgentFontName" "AgentFontSize" "AgentFontStyle"
    "AgentStereotypeFontColor" "AgentStereotypeFontName"
    "AgentStereotypeFontSize" "AgentStereotypeFontStyle"
    "ArchimateBorderThickness" "ArchimateFontColor" "ArchimateFontName"
    "ArchimateFontSize" "ArchimateFontStyle"
    "ArchimateStereotypeFontColor" "ArchimateStereotypeFontName"
    "ArchimateStereotypeFontSize" "ArchimateStereotypeFontStyle"
    "ArrowFontColor" "ArrowFontName" "ArrowFontSize" "ArrowFontStyle"
    "ArrowHeadColor" "ArrowLollipopColor" "ArrowMessageAlignment"
    "ArrowThickness" "ArtifactFontColor" "ArtifactFontName"
    "ArtifactFontSize" "ArtifactFontStyle" "ArtifactStereotypeFontColor"
    "ArtifactStereotypeFontName" "ArtifactStereotypeFontSize"
    "ArtifactStereotypeFontStyle" "BackgroundColor"
    "BiddableBackgroundColor" "BiddableBorderColor" "BoundaryFontColor"
    "BoundaryFontName" "BoundaryFontSize" "BoundaryFontStyle"
    "BoundaryStereotypeFontColor" "BoundaryStereotypeFontName"
    "BoundaryStereotypeFontSize" "BoundaryStereotypeFontStyle"
    "BoxPadding" "CaptionFontColor" "CaptionFontName" "CaptionFontSize"
    "CaptionFontStyle" "CardBorderThickness" "CardFontColor"
    "CardFontName" "CardFontSize" "CardFontStyle"
    "CardStereotypeFontColor" "CardStereotypeFontName"
    "CardStereotypeFontSize" "CardStereotypeFontStyle"
    "CircledCharacterFontColor" "CircledCharacterFontName"
    "CircledCharacterFontSize" "CircledCharacterFontStyle"
    "CircledCharacterRadius" "ClassAttributeFontColor"
    "ClassAttributeFontName" "ClassAttributeFontSize"
    "ClassAttributeFontStyle" "ClassAttributeIconSize"
    "ClassBackgroundColor" "ClassBorderColor" "ClassBorderThickness"
    "ClassFontColor" "ClassFontName" "ClassFontSize" "ClassFontStyle"
    "ClassStereotypeFontColor" "ClassStereotypeFontName"
    "ClassStereotypeFontSize" "ClassStereotypeFontStyle"
    "CloudFontColor" "CloudFontName" "CloudFontSize" "CloudFontStyle"
    "CloudStereotypeFontColor" "CloudStereotypeFontName"
    "CloudStereotypeFontSize" "CloudStereotypeFontStyle"
    "ColorArrowSeparationSpace" "ComponentBorderThickness"
    "ComponentFontColor" "ComponentFontName" "ComponentFontSize"
    "ComponentFontStyle" "ComponentStereotypeFontColor"
    "ComponentStereotypeFontName" "ComponentStereotypeFontSize"
    "ComponentStereotypeFontStyle" "ComponentStyle" "ConditionEndStyle"
    "ConditionStyle" "ControlFontColor" "ControlFontName"
    "ControlFontSize" "ControlFontStyle" "ControlStereotypeFontColor"
    "ControlStereotypeFontName" "ControlStereotypeFontSize"
    "ControlStereotypeFontStyle" "DatabaseFontColor" "DatabaseFontName"
    "DatabaseFontSize" "DatabaseFontStyle" "DatabaseStereotypeFontColor"
    "DatabaseStereotypeFontName" "DatabaseStereotypeFontSize"
    "DatabaseStereotypeFontStyle" "DefaultFontColor" "DefaultFontName"
    "DefaultFontSize" "DefaultFontStyle" "DefaultMonospacedFontName"
    "DefaultTextAlignment" "DesignedBackgroundColor"
    "DesignedBorderColor" "DesignedDomainBorderThickness"
    "DesignedDomainFontColor" "DesignedDomainFontName"
    "DesignedDomainFontSize" "DesignedDomainFontStyle"
    "DesignedDomainStereotypeFontColor"
    "DesignedDomainStereotypeFontName"
    "DesignedDomainStereotypeFontSize"
    "DesignedDomainStereotypeFontStyle" "DiagramBorderColor"
    "DiagramBorderThickness" "DomainBackgroundColor" "DomainBorderColor"
    "DomainBorderThickness" "DomainFontColor" "DomainFontName"
    "DomainFontSize" "DomainFontStyle" "DomainStereotypeFontColor"
    "DomainStereotypeFontName" "DomainStereotypeFontSize"
    "DomainStereotypeFontStyle" "Dpi" "EntityFontColor" "EntityFontName"
    "EntityFontSize" "EntityFontStyle" "EntityStereotypeFontColor"
    "EntityStereotypeFontName" "EntityStereotypeFontSize"
    "EntityStereotypeFontStyle" "FileFontColor" "FileFontName"
    "FileFontSize" "FileFontStyle" "FileStereotypeFontColor"
    "FileStereotypeFontName" "FileStereotypeFontSize"
    "FileStereotypeFontStyle" "FixCircleLabelOverlapping"
    "FolderFontColor" "FolderFontName" "FolderFontSize"
    "FolderFontStyle" "FolderStereotypeFontColor"
    "FolderStereotypeFontName" "FolderStereotypeFontSize"
    "FolderStereotypeFontStyle" "FooterFontColor" "FooterFontName"
    "FooterFontSize" "FooterFontStyle" "FrameFontColor" "FrameFontName"
    "FrameFontSize" "FrameFontStyle" "FrameStereotypeFontColor"
    "FrameStereotypeFontName" "FrameStereotypeFontSize"
    "FrameStereotypeFontStyle" "GenericDisplay" "Guillemet"
    "Handwritten" "HeaderFontColor" "HeaderFontName" "HeaderFontSize"
    "HeaderFontStyle" "HexagonBorderThickness" "HexagonFontColor"
    "HexagonFontName" "HexagonFontSize" "HexagonFontStyle"
    "HexagonStereotypeFontColor" "HexagonStereotypeFontName"
    "HexagonStereotypeFontSize" "HexagonStereotypeFontStyle"
    "HyperlinkColor" "HyperlinkUnderline" "IconIEMandatoryColor"
    "IconPackageBackgroundColor" "IconPackageColor"
    "IconPrivateBackgroundColor" "IconPrivateColor"
    "IconProtectedBackgroundColor" "IconProtectedColor"
    "IconPublicBackgroundColor" "IconPublicColor" "InterfaceFontColor"
    "InterfaceFontName" "InterfaceFontSize" "InterfaceFontStyle"
    "InterfaceStereotypeFontColor" "InterfaceStereotypeFontName"
    "InterfaceStereotypeFontSize" "InterfaceStereotypeFontStyle"
    "LabelFontColor" "LabelFontName" "LabelFontSize" "LabelFontStyle"
    "LabelStereotypeFontColor" "LabelStereotypeFontName"
    "LabelStereotypeFontSize" "LabelStereotypeFontStyle"
    "LegendBorderThickness" "LegendFontColor" "LegendFontName"
    "LegendFontSize" "LegendFontStyle" "LexicalBackgroundColor"
    "LexicalBorderColor" "LifelineStrategy" "Linetype"
    "MachineBackgroundColor" "MachineBorderColor"
    "MachineBorderThickness" "MachineFontColor" "MachineFontName"
    "MachineFontSize" "MachineFontStyle" "MachineStereotypeFontColor"
    "MachineStereotypeFontName" "MachineStereotypeFontSize"
    "MachineStereotypeFontStyle" "MaxAsciiMessageLength"
    "MaxMessageSize" "MinClassWidth" "Monochrome" "NodeFontColor"
    "NodeFontName" "NodeFontSize" "NodeFontStyle"
    "NodeStereotypeFontColor" "NodeStereotypeFontName"
    "NodeStereotypeFontSize" "NodeStereotypeFontStyle" "Nodesep"
    "NoteBackgroundColor" "NoteBorderColor" "NoteBorderThickness"
    "NoteFontColor" "NoteFontName" "NoteFontSize" "NoteFontStyle"
    "NoteShadowing" "NoteTextAlignment" "ObjectAttributeFontColor"
    "ObjectAttributeFontName" "ObjectAttributeFontSize"
    "ObjectAttributeFontStyle" "ObjectBorderThickness" "ObjectFontColor"
    "ObjectFontName" "ObjectFontSize" "ObjectFontStyle"
    "ObjectStereotypeFontColor" "ObjectStereotypeFontName"
    "ObjectStereotypeFontSize" "ObjectStereotypeFontStyle"
    "PackageBorderThickness" "PackageFontColor" "PackageFontName"
    "PackageFontSize" "PackageFontStyle" "PackageStereotypeFontColor"
    "PackageStereotypeFontName" "PackageStereotypeFontSize"
    "PackageStereotypeFontStyle" "PackageStyle" "PackageTitleAlignment"
    "Padding" "PageBorderColor" "PageExternalColor" "PageMargin"
    "ParticipantFontColor" "ParticipantFontName" "ParticipantFontSize"
    "ParticipantFontStyle" "ParticipantPadding"
    "ParticipantStereotypeFontColor" "ParticipantStereotypeFontName"
    "ParticipantStereotypeFontSize" "ParticipantStereotypeFontStyle"
    "PartitionBorderThickness" "PartitionFontColor" "PartitionFontName"
    "PartitionFontSize" "PartitionFontStyle" "PathHoverColor"
    "PersonBorderThickness" "PersonFontColor" "PersonFontName"
    "PersonFontSize" "PersonFontStyle" "PersonStereotypeFontColor"
    "PersonStereotypeFontName" "PersonStereotypeFontSize"
    "PersonStereotypeFontStyle" "QueueBorderThickness" "QueueFontColor"
    "QueueFontName" "QueueFontSize" "QueueFontStyle"
    "QueueStereotypeFontColor" "QueueStereotypeFontName"
    "QueueStereotypeFontSize" "QueueStereotypeFontStyle" "Ranksep"
    "RectangleBorderThickness" "RectangleFontColor" "RectangleFontName"
    "RectangleFontSize" "RectangleFontStyle"
    "RectangleStereotypeFontColor" "RectangleStereotypeFontName"
    "RectangleStereotypeFontSize" "RectangleStereotypeFontStyle"
    "RequirementBackgroundColor" "RequirementBorderColor"
    "RequirementBorderThickness" "RequirementFontColor"
    "RequirementFontName" "RequirementFontSize" "RequirementFontStyle"
    "RequirementStereotypeFontColor" "RequirementStereotypeFontName"
    "RequirementStereotypeFontSize" "RequirementStereotypeFontStyle"
    "ResponseMessageBelowArrow" "RoundCorner" "SameClassWidth"
    "SequenceActorBorderThickness" "SequenceArrowThickness"
    "SequenceBoxBorderColor" "SequenceBoxFontColor"
    "SequenceBoxFontName" "SequenceBoxFontSize" "SequenceBoxFontStyle"
    "SequenceDelayFontColor" "SequenceDelayFontName"
    "SequenceDelayFontSize" "SequenceDelayFontStyle"
    "SequenceDividerBorderThickness" "SequenceDividerFontColor"
    "SequenceDividerFontName" "SequenceDividerFontSize"
    "SequenceDividerFontStyle" "SequenceGroupBodyBackgroundColor"
    "SequenceGroupBorderThickness" "SequenceGroupFontColor"
    "SequenceGroupFontName" "SequenceGroupFontSize"
    "SequenceGroupFontStyle" "SequenceGroupHeaderFontColor"
    "SequenceGroupHeaderFontName" "SequenceGroupHeaderFontSize"
    "SequenceGroupHeaderFontStyle" "SequenceLifeLineBorderColor"
    "SequenceLifeLineBorderThickness" "SequenceMessageAlignment"
    "SequenceMessageTextAlignment" "SequenceNewpageSeparatorColor"
    "SequenceParticipant" "SequenceParticipantBorderThickness"
    "SequenceReferenceAlignment" "SequenceReferenceBackgroundColor"
    "SequenceReferenceBorderThickness" "SequenceReferenceFontColor"
    "SequenceReferenceFontName" "SequenceReferenceFontSize"
    "SequenceReferenceFontStyle"
    "SequenceReferenceHeaderBackgroundColor"
    "SequenceStereotypeFontColor" "SequenceStereotypeFontName"
    "SequenceStereotypeFontSize" "SequenceStereotypeFontStyle"
    "Shadowing" "StackFontColor" "StackFontName" "StackFontSize"
    "StackFontStyle" "StackStereotypeFontColor"
    "StackStereotypeFontName" "StackStereotypeFontSize"
    "StackStereotypeFontStyle" "StateAttributeFontColor"
    "StateAttributeFontName" "StateAttributeFontSize"
    "StateAttributeFontStyle" "StateBorderColor" "StateFontColor"
    "StateFontName" "StateFontSize" "StateFontStyle"
    "StateMessageAlignment" "StereotypePosition" "StorageFontColor"
    "StorageFontName" "StorageFontSize" "StorageFontStyle"
    "StorageStereotypeFontColor" "StorageStereotypeFontName"
    "StorageStereotypeFontSize" "StorageStereotypeFontStyle" "Style"
    "SvglinkTarget" "SwimlaneBorderThickness" "SwimlaneTitleFontColor"
    "SwimlaneTitleFontName" "SwimlaneTitleFontSize"
    "SwimlaneTitleFontStyle" "SwimlaneWidth" "SwimlaneWrapTitleWidth"
    "TabSize" "TimingFontColor" "TimingFontName" "TimingFontSize"
    "TimingFontStyle" "TitleBorderRoundCorner" "TitleBorderThickness"
    "TitleFontColor" "TitleFontName" "TitleFontSize" "TitleFontStyle"
    "UsecaseBorderThickness" "UsecaseFontColor" "UsecaseFontName"
    "UsecaseFontSize" "UsecaseFontStyle" "UsecaseStereotypeFontColor"
    "UsecaseStereotypeFontName" "UsecaseStereotypeFontSize"
    "UsecaseStereotypeFontStyle" "WrapWidth")
  "Planuml skin parameters")

(defconst plantuml--preprocessor-keywords
  '("assert" "define" "definelong" "dump_memory" "else"
    "enddefinelong" "endfunction" "endif" "endprocedure" "endsub"
    "exit" "function" "if" "ifdef" "ifndef" "import" "include" "local"
    "log" "pragma" "procedure" "return" "startsub" "theme" "undef"
    "unquoted")
  "Plantuml preprocessor instructions")

(defconst plantuml--sequence-arrows
  '("->x" "->" "->>" "-\\" "\\\\-" "//--" "->o" "o\\\\--" "<->" "<->o"))

;;; font-lock-keywords
(defconst plantuml--font-lock-components
  (list
   ;; regexp
   (regexp-opt plantuml--component-types)
   ;; font-face
   '(0 font-lock-type-face)))

(defconst plantuml--font-lock-diagrams
  (list
   ;; regexp
   (rx-to-string `(seq "a" (or "start" "end") (or ,@plantuml--diagram-types)))
   ;; font-face
   '(0 font-lock-builtin-face)))

(defconst plantuml--font-lock-preprocessor
  (list
   ;; regexp
   (rx-to-string `(seq "!" (or ,@plantuml--preprocessor-keywords)))
   ;; font-face
   '(0 font-lock-preprocessor-face)))

;;; font-lock-keywords
(defconst plantuml--font-lock-components
  (list
   ;; regexp
   (regexp-opt plantuml--component-types)
   ;; font-face
   '(0 font-lock-type-face)))

(defconst plantuml--font-lock-diagrams
  (list
   ;; regexp
   (rx-to-string `(seq "@" (or "start" "end") (or ,@plantuml--diagram-types)))
   ;; font-face
   '(0 font-lock-builtin-face)))

(defconst plantuml--font-lock-preprocessor
  (list
   ;; regexp
   (rx-to-string `(seq "!" (or ,@plantuml--preprocessor-keywords)))
   ;; font-face
   '(0 font-lock-preprocessor-face)))

(defconst plantuml--font-lock-keywords
  (list
   ;; regexp
   (concat "\\<" (regexp-opt plantuml--keywords) "|\>" )
   ;; font-face
   '(0 font-lock-keyword-face)))

(defconst plantuml--font-lock-skin-parameters
  (list
   ;; regexp
   (rx-to-string
    `(seq "skinparam"
	  (one-or-more whitespace)
	  (group (or ,@plantuml--skin-parameters))
	  (one-or-more whitespace)
	  (group (one-or-more word))
	  eol))
   ;; font-face
   '(1 font-lock-variable-name-face)
   '(2 font-lock-constant-face)))

(defconst plantuml--font-lock-sequence-connectors
  (list
   (rx-to-string
    `(seq bol (+ whitespace)
	  (group (+ (or word "_")))
	  (*? whitespace)
	  (group (seq (or "?-" "[-" "_" "\\\\" "//" "o\\\\" "<" ">")
		      (*? (seq "[#" (or (repeat 6 hex-digit) (or ,@plantuml--color-names)) "]"))
		      (or ">" ">>" "\\-" "-" "--"  ">o" "->" "->o" ">]" ">?")
		      ))
	  (*? whitespace)
	  (group (+ (or word "_")))
	  (*? whitespace)
	  ":"
	  (+? whitespace)
	  (group (* any))
	  ))
   ;; faces
   '(1 font-lock-constant-face)
   '(2 font-lock-warning-face)
   '(3 font-lock-constant-face)
   '(4 font-lock-string-face))
  "Face definition for sequence diagram syntax of the form Alice
-> Bob: Secret")

(defconst plantuml--font-lock-mindmap-headers
  (mapcar
   (lambda (x)
     (list
      ;; regexp
      (rx-to-string `(seq bol (repeat ,x (or "*" "+" "-"))
			  (? "[" (group "#" (or (repeat 6 digit) (or ,@plantuml--color-names))) "]")
			  (? (not (any "*" "_" "+" )))
			  (+ space)
			  (group (* any))))
      ;; face sexp
      `(2 (if (match-string-no-properties 1)
	      (add-text-properties (match-beginning 2) (match-end 2)
				   (list 'font-lock-face (list :foreground (plantuml--find-color (match-string-no-properties 1)))))
	    ,(format "outline-%d" x)))
      ))
   (number-sequence 1 8))
  "Face definitions for mindmap headers")

(defun add-font-lock-face-props (start end props)
  (let ((existing-face-props (plist-get (text-properties-at start) 'font-lock-face)))
    (add-text-properties
     start end
     (list 'font-lock-face (append existing-face-props props)))))

;; creole
(defconst plantuml--font-lock-creole-bold
  '("\\*\\*[^\n]+?\\*\\*"
    (0 (add-font-lock-face-props (match-beginning 0) (match-end 0) '(:weight bold)))))

(defconst plantuml--font-lock-separator
  '("==[^\n]+?=="
    (0 (add-text-properties (match-beginning 0) (match-end 0) '(font-lock-face (:weight bold :height 1 :width expanded))))))

(defconst plantuml--font-lock-creole-italic
  '("//[^\nn]+?//"
    (0 (add-font-lock-face-props (match-beginning 0) (match-end 0) '(:slant italics)))))

(defconst plantuml--font-lock-creole-underline
  '("__[^\n]+?__"
    (0 (add-font-lock-face-props (match-beginning 0) (match-end 0) '(:underline t)))))

(defconst plantuml--font-lock-creole-monospace
  '("\"\"[^\n]+?\"\"" . 'header-line))

(defconst plantuml--font-lock-creole-strikethrough
  '("--[^\n]+?--"
    (0 (add-font-lock-face-props (match-beginning 0) (match-end 0) '(:strike-through t)))))

(defconst planutml--font-lock-alias
  '("\\<as[[:space:]]+\\([[:alnum:]_]+\\)[[:space:]]*$"
    (1 font-lock-variable-name-face)))

(defconst plantuml--font-lock-condition
  '("(\\([^\n)]+?\\))" . 'font-lock-string-face))

(defconst plantuml--font-lock-swimlane
  '("|\\(?:\\([^|\n]+|\\)?\\([^|\n]+\\)\\)|" . 'lazy-highlight))

(defconst plantuml--font-lock-sequence-arrows
  (cons
   (regexp-opt plantuml--sequence-arrows)
   'font-lock-warning-face))


(defconst plantuml--font-lock-defaults
  (list
   ;; group syntax
   plantuml--font-lock-skin-parameters
   planutml--font-lock-alias
   plantuml--font-lock-sequence-connectors
   ;; keywords
   plantuml--font-lock-diagrams
   plantuml--font-lock-preprocessor
   plantuml--font-lock-components
   plantuml--font-lock-keywords
   ;; special syntax
   plantuml--font-lock-swimlane
   plantuml--font-lock-condition
   plantuml--font-lock-sequence-arrows
   plantuml--font-lock-separator
   ;; creole
   plantuml--font-lock-creole-bold
   plantuml--font-lock-creole-italic
   plantuml--font-lock-creole-underline
   plantuml--font-lock-creole-monospace
   plantuml--font-lock-creole-strikethrough
   ))

;;;###autoload
(define-derived-mode plantuml-mode
  prog-mode "plantuml" "Major mode for plantuml"
  (setq
   font-lock-defaults '((plantuml--font-lock-defaults) nil t)
   plantuml-java-cmd (get-java-cmd)
   plantuml-jar-path (get-plantuml-jar))
  (font-lock-add-keywords 'plantuml-mode plantuml--font-lock-mindmap-headers)
  (add-hook 'xref-backend-functions #'plantuml-xref-backend)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode) t)

;;; preview helper functions
(defun get-java-cmd ()
  "Returns absolute path to the java executable assuming that the
JAVA HOME environment variable is set"
  (expand-file-name
   "bin/java"
   (let ((java-home (getenv "JAVA_HOME" )))
     (when (string-prefix-p "%" java-home)
       (getenv (replace-regexp-in-string "%" "" java-home)))
     )))

(require 'seq)
(defun get-plantuml-jar ()
  "Returns absolute path to plantuml jar file assuming APPS_HOME
environment variable is defined and only one jar file is present
in the $APPS_HOME/plantuml folder"
  (let ((plantuml-home (expand-file-name "plantuml" (getenv "APPS_HOME" )))
	(find-jar (lambda (x) (string-suffix-p ".jar" x) )))
    (let ((plantuml-jar (car
			 (seq-filter
			  find-jar
			  (directory-files plantuml-home)))))
      (when plantuml-jar
	(expand-file-name plantuml-jar plantuml-home)))))

(defun plantuml--preview-process (filename)
  "Launch process to generate preview and return the running
process"
  (let ((default-directory (file-name-directory filename))
	(basename (file-name-nondirectory filename)))
    (start-process
     "*plantuml*" "*plantuml*" plantum-java-cmd
     "-jar" plantuml-jar-path basename)))

(defun plantuml--preview-process-sentinel (proc event)
  "Sentinel function to print status of preview process"
  (if (equal "finished\n" event)
      (message "Generated preview successfully")
    (user-error "Generating preview failed")))

(defun plantuml-preview ()
  "Spawn a process to generate png preview file. Use with universal
argument (C-u) to remove any existing preview png file."
  (interactive)
  (when current-prefix-arg
    (let ((preview-file
	   (expand-file-name
	    (concat (file-name-base (buffer-file-name)) ".png")
	    (file-name-directory (buffer-file-name)))))
      (when (file-exists-p preview-file)
	(delete-file preview-file))))
  (let ((filename (buffer-file-name)))
    (if (file-exists-p filename)
	(let ((ps (plantuml--preview-process filename)))
	  (set-process-sentinel ps #'plantuml--preview-process-sentinel))
      (user-error "File not present. Save file before previewing"))))

(defun platnuml--open-paint (filename)
  "Launch MS-Paint to preview generated png file"
  (let ((default-directory (file-name-directory filename))
	(target-file (file-name-nondirectory filename)))
    (start-process "*plantuml-open-preview*" nil "mspaint" target-file)))

(defun plantuml-open-preview ()
  "Open generated preview png file in MS-Paint"
  (interactive)
  (let ((filename expand-file-name
		  (concat (file-name-base (buffer-file-name)) ".png")
		  (file-name-directory (buffer-file-name)))))
  (if (file-exists-p filename)
      (platnuml--open-paint filename)))

;; bootstrap template functions
(defun plantuml--select-diagram-prompt ()
  "Prompt string listing diagram types for selection"
  (concat
   (mapconcat
    (lambda (x) (format "%s -\t%s" (car x) (cadr x)))
    plantuml-diagram-types "\n")
   "\nSelect diagram type: "))

(defun plantuml-select-diagram ()
  "Insert bootstrap template based on the diagram type selected"
  (interactive)
  (let* ((arg (read-string (plantuml--select-diagram-prompt) "u" nil "u"))
	 (diagram (assoc arg plantuml-diagram-types)))
    (when diagram
      (let* ((suffix (car (last diagram)))
	     (start (format "@start%s\n" suffix)))
	(goto-char 0)
	(insert (format "%s\n@end%s\n" start suffix))
	(goto-char (1+ (length start)))))))


;; Define ref backend
(require 'cl-lib)
(require 'xref)

(defun plantum-xref-backend ()
  "xref backend for plantuml files"
  'plantuml)
(cl-defmethod ref-backend-identifier-at-point ((backend (eql plantuml)))
  "Return the identifier to lookup"
;; (message "plantuml-xref (%s): Identifier %s (point=%d)" (buffer-name) (symbol-at-point) ( point ))
  (symbol-name (symbol-at-point))
  )
;; xref-backend
(cl-defmethod ref-backend-identifier-completion-table ((backend (eql plantuml)))
  "Return list of terms for completion from the current buffer"
  (plantuml--find-definitions nil))
(cl-defmethod ref-backend-definitions ((backend (eql plantuml)) symbol)
  ;; (message "plantuml-xref (%s) : %s" (buffer-name) symbol)
  (plantuml--find-definitions symbol t))
(cl-defmethod ref-backend-references ((backend (eql plantuml)) symbol)
  "List of references matching symbol"
  (plantuml--find-definitions symbol t))
;; xref helper funtion
(defun plantuml--find-definitions (symbol &optional ref)
  "Find definitions in buffer if 'REF' is + retun matches"
  (let ((case-fold-search t)
	(regexp (if symbol
		    (rx-to-string `(seq bow (or ,@plantuml--component-types) eow (+ space) bow (group ,symbol) eow))
		  (rx-to-string `(seq bow (or ,@plantuml--component-types) eow (+ space) bow (group (+ (any word "_"))) eow)))))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let (matches)
	  (while (re-search-forward regexp nil t)
	    (push (if ref
		      (xref-make
		       (buffer-substring-no-properties (line-beginning-position) (line-end-position))
		       (xref-make-buffer-location (current-buffer) (match-beginning 1)))
		    (match-string-no-properties 1))
		  matches))
	  matches)
	))
    )
  )

;;; debugging
(defun eval-file ()
  (interactive)
  (eval-buffer (current-buffer))
  (user-error "Evaluated plantuml-mode"))

(local-set-key (kbd "C-c C-c") #'eval-file)
