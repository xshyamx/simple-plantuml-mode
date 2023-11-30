;;; plantuml-keywords.el -- Plantuml keyword constants -*- lexical-binding: t -*-

;; most of the following constants are extracted from output of
;; 'java -jar plantuml.jar -language'
(defconst plantuml--component-types
  '("abstract" "actor" "agent" "annotation" "archimate" "artifact"
    "boundary" "card" "class" "cloud" "collections" "component"
    "control" "database" "diamond" "entity" "enum" "file" "folder"
    "frame" "hexagon" "interface" "label" "node" "object" "package"
    "participant" "person" "queue" "rectangle" "stack" "state"
    "storage" "usecase" )
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
    "footer" "fork" "group" "header" "hide" "hnote" "if" "is" "italic"
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
    "ActorBorderColor" "ActorFontColor" "ActorFontName"
    "ActorFontSize" "ActorFontStyle" "ActorStereotypeFontColor"
    "ActorStereotypeFontName" "ActorStereotypeFontSize"
    "ActorStereotypeFontStyle" "AgentBorderThickness" "AgentFontColor"
    "AgentFontName" "AgentFontSize" "AgentFontStyle"
    "AgentStereotypeFontColor" "AgentStereotypeFontName"
    "AgentStereotypeFontSize" "AgentStereotypeFontStyle"
    "ArchimateBorderThickness" "ArchimateFontColor"
    "ArchimateFontName" "ArchimateFontSize" "ArchimateFontStyle"
    "ArchimateStereotypeFontColor" "ArchimateStereotypeFontName"
    "ArchimateStereotypeFontSize" "ArchimateStereotypeFontStyle"
    "ArrowFontColor" "ArrowFontName" "ArrowFontSize" "ArrowFontStyle"
    "ArrowHeadColor" "ArrowLollipopColor" "ArrowMessageAlignment"
    "ArrowThickness" "ArtifactFontColor" "ArtifactFontName"
    "ArtifactFontSize" "ArtifactFontStyle"
    "ArtifactStereotypeFontColor" "ArtifactStereotypeFontName"
    "ArtifactStereotypeFontSize" "ArtifactStereotypeFontStyle"
    "BackgroundColor" "BiddableBackgroundColor" "BiddableBorderColor"
    "BoundaryFontColor" "BoundaryFontName" "BoundaryFontSize"
    "BoundaryFontStyle" "BoundaryStereotypeFontColor"
    "BoundaryStereotypeFontName" "BoundaryStereotypeFontSize"
    "BoundaryStereotypeFontStyle" "BoxPadding" "CaptionFontColor"
    "CaptionFontName" "CaptionFontSize" "CaptionFontStyle"
    "CardBorderThickness" "CardFontColor" "CardFontName"
    "CardFontSize" "CardFontStyle" "CardStereotypeFontColor"
    "CardStereotypeFontName" "CardStereotypeFontSize"
    "CardStereotypeFontStyle" "CircledCharacterFontColor"
    "CircledCharacterFontName" "CircledCharacterFontSize"
    "CircledCharacterFontStyle" "CircledCharacterRadius"
    "ClassAttributeFontColor" "ClassAttributeFontName"
    "ClassAttributeFontSize" "ClassAttributeFontStyle"
    "ClassAttributeIconSize" "ClassBackgroundColor" "ClassBorderColor"
    "ClassBorderThickness" "ClassFontColor" "ClassFontName"
    "ClassFontSize" "ClassFontStyle" "ClassStereotypeFontColor"
    "ClassStereotypeFontName" "ClassStereotypeFontSize"
    "ClassStereotypeFontStyle" "CloudFontColor" "CloudFontName"
    "CloudFontSize" "CloudFontStyle" "CloudStereotypeFontColor"
    "CloudStereotypeFontName" "CloudStereotypeFontSize"
    "CloudStereotypeFontStyle" "ColorArrowSeparationSpace"
    "ComponentBorderThickness" "ComponentFontColor"
    "ComponentFontName" "ComponentFontSize" "ComponentFontStyle"
    "ComponentStereotypeFontColor" "ComponentStereotypeFontName"
    "ComponentStereotypeFontSize" "ComponentStereotypeFontStyle"
    "ComponentStyle" "ConditionEndStyle" "ConditionStyle"
    "ControlFontColor" "ControlFontName" "ControlFontSize"
    "ControlFontStyle" "ControlStereotypeFontColor"
    "ControlStereotypeFontName" "ControlStereotypeFontSize"
    "ControlStereotypeFontStyle" "DatabaseFontColor"
    "DatabaseFontName" "DatabaseFontSize" "DatabaseFontStyle"
    "DatabaseStereotypeFontColor" "DatabaseStereotypeFontName"
    "DatabaseStereotypeFontSize" "DatabaseStereotypeFontStyle"
    "DefaultFontColor" "DefaultFontName" "DefaultFontSize"
    "DefaultFontStyle" "DefaultMonospacedFontName"
    "DefaultTextAlignment" "DesignedBackgroundColor"
    "DesignedBorderColor" "DesignedDomainBorderThickness"
    "DesignedDomainFontColor" "DesignedDomainFontName"
    "DesignedDomainFontSize" "DesignedDomainFontStyle"
    "DesignedDomainStereotypeFontColor"
    "DesignedDomainStereotypeFontName"
    "DesignedDomainStereotypeFontSize"
    "DesignedDomainStereotypeFontStyle" "DiagramBorderColor"
    "DiagramBorderThickness" "DomainBackgroundColor"
    "DomainBorderColor" "DomainBorderThickness" "DomainFontColor"
    "DomainFontName" "DomainFontSize" "DomainFontStyle"
    "DomainStereotypeFontColor" "DomainStereotypeFontName"
    "DomainStereotypeFontSize" "DomainStereotypeFontStyle" "Dpi"
    "EntityFontColor" "EntityFontName" "EntityFontSize"
    "EntityFontStyle" "EntityStereotypeFontColor"
    "EntityStereotypeFontName" "EntityStereotypeFontSize"
    "EntityStereotypeFontStyle" "FileFontColor" "FileFontName"
    "FileFontSize" "FileFontStyle" "FileStereotypeFontColor"
    "FileStereotypeFontName" "FileStereotypeFontSize"
    "FileStereotypeFontStyle" "FixCircleLabelOverlapping"
    "FolderFontColor" "FolderFontName" "FolderFontSize"
    "FolderFontStyle" "FolderStereotypeFontColor"
    "FolderStereotypeFontName" "FolderStereotypeFontSize"
    "FolderStereotypeFontStyle" "FooterFontColor" "FooterFontName"
    "FooterFontSize" "FooterFontStyle" "FrameFontColor"
    "FrameFontName" "FrameFontSize" "FrameFontStyle"
    "FrameStereotypeFontColor" "FrameStereotypeFontName"
    "FrameStereotypeFontSize" "FrameStereotypeFontStyle"
    "GenericDisplay" "Guillemet" "Handwritten" "HeaderFontColor"
    "HeaderFontName" "HeaderFontSize" "HeaderFontStyle"
    "HexagonBorderThickness" "HexagonFontColor" "HexagonFontName"
    "HexagonFontSize" "HexagonFontStyle" "HexagonStereotypeFontColor"
    "HexagonStereotypeFontName" "HexagonStereotypeFontSize"
    "HexagonStereotypeFontStyle" "HyperlinkColor" "HyperlinkUnderline"
    "IconIEMandatoryColor" "IconPackageBackgroundColor"
    "IconPackageColor" "IconPrivateBackgroundColor" "IconPrivateColor"
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
    "ObjectAttributeFontStyle" "ObjectBorderThickness"
    "ObjectFontColor" "ObjectFontName" "ObjectFontSize"
    "ObjectFontStyle" "ObjectStereotypeFontColor"
    "ObjectStereotypeFontName" "ObjectStereotypeFontSize"
    "ObjectStereotypeFontStyle" "PackageBorderThickness"
    "PackageFontColor" "PackageFontName" "PackageFontSize"
    "PackageFontStyle" "PackageStereotypeFontColor"
    "PackageStereotypeFontName" "PackageStereotypeFontSize"
    "PackageStereotypeFontStyle" "PackageStyle"
    "PackageTitleAlignment" "Padding" "PageBorderColor"
    "PageExternalColor" "PageMargin" "ParticipantFontColor"
    "ParticipantFontName" "ParticipantFontSize" "ParticipantFontStyle"
    "ParticipantPadding" "ParticipantStereotypeFontColor"
    "ParticipantStereotypeFontName" "ParticipantStereotypeFontSize"
    "ParticipantStereotypeFontStyle" "PartitionBorderThickness"
    "PartitionFontColor" "PartitionFontName" "PartitionFontSize"
    "PartitionFontStyle" "PathHoverColor" "PersonBorderThickness"
    "PersonFontColor" "PersonFontName" "PersonFontSize"
    "PersonFontStyle" "PersonStereotypeFontColor"
    "PersonStereotypeFontName" "PersonStereotypeFontSize"
    "PersonStereotypeFontStyle" "QueueBorderThickness"
    "QueueFontColor" "QueueFontName" "QueueFontSize" "QueueFontStyle"
    "QueueStereotypeFontColor" "QueueStereotypeFontName"
    "QueueStereotypeFontSize" "QueueStereotypeFontStyle" "Ranksep"
    "RectangleBorderThickness" "RectangleFontColor"
    "RectangleFontName" "RectangleFontSize" "RectangleFontStyle"
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

(provide 'plantuml-keywords)
;;; plantuml-keywords.el -- Ends here