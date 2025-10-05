;;; plantuml-stdlib-aws.el --- PlantUML AWS components  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Components extracted from https://github.com/awslabs/aws-icons-for-plantuml on tag `v14.0'

;;; Code:

(require 'cl-generic)

(defconst plantuml-aws-components
  '(("ApplicationCostProfiler"
     . "CloudFinancialManagement/ApplicationCostProfiler")
    ("ApplicationCostProfilerParticipant"
     . "CloudFinancialManagement/ApplicationCostProfiler")
    ("GameTech" . "GameTech/GameTech")
    ("GameTechParticipant" . "GameTech/GameTech")
    ("Amplify" . "FrontEndWebMobile/Amplify")
    ("AmplifyParticipant" . "FrontEndWebMobile/Amplify")
    ("IQ" . "CustomerEnablement/IQ")
    ("IQParticipant" . "CustomerEnablement/IQ")
    ("AuroraSQLServerInstanceAlternate"
     . "Database/AuroraSQLServerInstanceAlternate")
    ("AuroraSQLServerInstanceAlternateParticipant"
     . "Database/AuroraSQLServerInstanceAlternate")
    ("MainframeModernizationCompiler"
     . "MigrationTransfer/MainframeModernizationCompiler")
    ("MainframeModernizationCompilerParticipant"
     . "MigrationTransfer/MainframeModernizationCompiler")
    ("RoboMakerSimulation" . "Robotics/RoboMakerSimulation")
    ("RoboMakerSimulationParticipant" . "Robotics/RoboMakerSimulation")
    ("RedshiftRA3" . "Analytics/RedshiftRA3")
    ("RedshiftRA3Participant" . "Analytics/RedshiftRA3")
    ("BillingConductor" . "CloudFinancialManagement/BillingConductor")
    ("BillingConductorParticipant"
     . "CloudFinancialManagement/BillingConductor")
    ("ElasticBlockStoreVolumegp3" . "Storage/ElasticBlockStoreVolumegp3")
    ("ElasticBlockStoreVolumegp3Participant"
     . "Storage/ElasticBlockStoreVolumegp3")
    ("GameLift" . "GameTech/GameLift")
    ("GameLiftParticipant" . "GameTech/GameLift")
    ("LocationServiceRoutes" . "FrontEndWebMobile/LocationServiceRoutes")
    ("LocationServiceRoutesParticipant"
     . "FrontEndWebMobile/LocationServiceRoutes")
    ("AuroraPostgreSQLInstanceAlternate"
     . "Database/AuroraPostgreSQLInstanceAlternate")
    ("AuroraPostgreSQLInstanceAlternateParticipant"
     . "Database/AuroraPostgreSQLInstanceAlternate")
    ("AmplifyAWSAmplifyStudio"
     . "FrontEndWebMobile/AmplifyAWSAmplifyStudio")
    ("AmplifyAWSAmplifyStudioParticipant"
     . "FrontEndWebMobile/AmplifyAWSAmplifyStudio")
    ("DynamoDBStream" . "Database/DynamoDBStream")
    ("DynamoDBStreamParticipant" . "Database/DynamoDBStream")
    ("ManagedServices" . "CustomerEnablement/ManagedServices")
    ("ManagedServicesParticipant" . "CustomerEnablement/ManagedServices")
    ("Serverless" . "Serverless/Serverless")
    ("ServerlessParticipant" . "Serverless/Serverless")
    ("MigrationEvaluator" . "MigrationTransfer/MigrationEvaluator")
    ("MigrationEvaluatorParticipant"
     . "MigrationTransfer/MigrationEvaluator")
    ("RoboMaker" . "Robotics/RoboMaker")
    ("RoboMakerParticipant" . "Robotics/RoboMaker")
    ("RedshiftDenseStorageNode" . "Analytics/RedshiftDenseStorageNode")
    ("RedshiftDenseStorageNodeParticipant"
     . "Analytics/RedshiftDenseStorageNode")
    ("Lumberyard" . "GameTech/Lumberyard")
    ("LumberyardParticipant" . "GameTech/Lumberyard")
    ("StorageGateway" . "Storage/StorageGateway")
    ("StorageGatewayParticipant" . "Storage/StorageGateway")
    ("LocationServiceMap" . "FrontEndWebMobile/LocationServiceMap")
    ("LocationServiceMapParticipant"
     . "FrontEndWebMobile/LocationServiceMap")
    ("ElastiCacheElastiCacheforRedis"
     . "Database/ElastiCacheElastiCacheforRedis")
    ("ElastiCacheElastiCacheforRedisParticipant"
     . "Database/ElastiCacheElastiCacheforRedis")
    ("FrontEndWebMobile" . "FrontEndWebMobile/FrontEndWebMobile")
    ("FrontEndWebMobileParticipant"
     . "FrontEndWebMobile/FrontEndWebMobile")
    ("DynamoDBAmazonDynamoDBAccelerator"
     . "Database/DynamoDBAmazonDynamoDBAccelerator")
    ("DynamoDBAmazonDynamoDBAcceleratorParticipant"
     . "Database/DynamoDBAmazonDynamoDBAccelerator")
    ("rePost" . "CustomerEnablement/rePost")
    ("rePostParticipant" . "CustomerEnablement/rePost")
    ("TransferFamilyAWSSFTP" . "MigrationTransfer/TransferFamilyAWSSFTP")
    ("TransferFamilyAWSSFTPParticipant"
     . "MigrationTransfer/TransferFamilyAWSSFTP")
    ("CloudSearchSearchDocuments"
     . "Analytics/CloudSearchSearchDocuments")
    ("CloudSearchSearchDocumentsParticipant"
     . "Analytics/CloudSearchSearchDocuments")
    ("CloudFinancialManagement"
     . "CloudFinancialManagement/CloudFinancialManagement")
    ("CloudFinancialManagementParticipant"
     . "CloudFinancialManagement/CloudFinancialManagement")
    ("GameSparks" . "GameTech/GameSparks")
    ("GameSparksParticipant" . "GameTech/GameSparks")
    ("BackupGateway" . "Storage/BackupGateway")
    ("BackupGatewayParticipant" . "Storage/BackupGateway")
    ("DeviceFarm" . "FrontEndWebMobile/DeviceFarm")
    ("DeviceFarmParticipant" . "FrontEndWebMobile/DeviceFarm")
    ("RDS" . "Database/RDS") ("RDSParticipant" . "Database/RDS")
    ("LocationServicePlace" . "FrontEndWebMobile/LocationServicePlace")
    ("LocationServicePlaceParticipant"
     . "FrontEndWebMobile/LocationServicePlace")
    ("RDSMultiAZ" . "Database/RDSMultiAZ")
    ("RDSMultiAZParticipant" . "Database/RDSMultiAZ")
    ("Support" . "CustomerEnablement/Support")
    ("SupportParticipant" . "CustomerEnablement/Support")
    ("Activate" . "CustomerEnablement/Activate")
    ("ActivateParticipant" . "CustomerEnablement/Activate")
    ("TransferFamily" . "MigrationTransfer/TransferFamily")
    ("TransferFamilyParticipant" . "MigrationTransfer/TransferFamily")
    ("QuickSight" . "Analytics/QuickSight")
    ("QuickSightParticipant" . "Analytics/QuickSight")
    ("RoboMakerCloudExtensionsROS"
     . "Robotics/RoboMakerCloudExtensionsROS")
    ("RoboMakerCloudExtensionsROSParticipant"
     . "Robotics/RoboMakerCloudExtensionsROS")
    ("ReservedInstanceReporting"
     . "CloudFinancialManagement/ReservedInstanceReporting")
    ("ReservedInstanceReportingParticipant"
     . "CloudFinancialManagement/ReservedInstanceReporting")
    ("FSxforNetAppONTAP" . "Storage/FSxforNetAppONTAP")
    ("FSxforNetAppONTAPParticipant" . "Storage/FSxforNetAppONTAP")
    ("LocationServiceGeofence"
     . "FrontEndWebMobile/LocationServiceGeofence")
    ("LocationServiceGeofenceParticipant"
     . "FrontEndWebMobile/LocationServiceGeofence")
    ("DynamoDBStandardInfrequentAccessTableClass"
     . "Database/DynamoDBStandardInfrequentAccessTableClass")
    ("DynamoDBStandardInfrequentAccessTableClassParticipant"
     . "Database/DynamoDBStandardInfrequentAccessTableClass")
    ("LocationService" . "FrontEndWebMobile/LocationService")
    ("LocationServiceParticipant" . "FrontEndWebMobile/LocationService")
    ("DynamoDBTable" . "Database/DynamoDBTable")
    ("DynamoDBTableParticipant" . "Database/DynamoDBTable")
    ("ProfessionalServices" . "CustomerEnablement/ProfessionalServices")
    ("ProfessionalServicesParticipant"
     . "CustomerEnablement/ProfessionalServices")
    ("TrainingCertification" . "CustomerEnablement/TrainingCertification")
    ("TrainingCertificationParticipant"
     . "CustomerEnablement/TrainingCertification")
    ("Redshift" . "Analytics/Redshift")
    ("RedshiftParticipant" . "Analytics/Redshift")
    ("MigrationHub" . "MigrationTransfer/MigrationHub")
    ("MigrationHubParticipant" . "MigrationTransfer/MigrationHub")
    ("RoboMakerDevelopmentEnvironment"
     . "Robotics/RoboMakerDevelopmentEnvironment")
    ("RoboMakerDevelopmentEnvironmentParticipant"
     . "Robotics/RoboMakerDevelopmentEnvironment")
    ("CostExplorer" . "CloudFinancialManagement/CostExplorer")
    ("CostExplorerParticipant" . "CloudFinancialManagement/CostExplorer")
    ("BackupBackupVault" . "Storage/BackupBackupVault")
    ("BackupBackupVaultParticipant" . "Storage/BackupBackupVault")
    ("Open3DEngine" . "GameTech/Open3DEngine")
    ("Open3DEngineParticipant" . "GameTech/Open3DEngine")
    ("AuroraAmazonRDSInstanceAternate"
     . "Database/AuroraAmazonRDSInstanceAternate")
    ("AuroraAmazonRDSInstanceAternateParticipant"
     . "Database/AuroraAmazonRDSInstanceAternate")
    ("AuroraPIOPSInstance" . "Database/AuroraPIOPSInstance")
    ("AuroraPIOPSInstanceParticipant" . "Database/AuroraPIOPSInstance")
    ("LocationServiceTrack" . "FrontEndWebMobile/LocationServiceTrack")
    ("LocationServiceTrackParticipant"
     . "FrontEndWebMobile/LocationServiceTrack")
    ("CustomerEnablement" . "CustomerEnablement/CustomerEnablement")
    ("CustomerEnablementParticipant"
     . "CustomerEnablement/CustomerEnablement")
    ("KinesisDataStreams" . "Analytics/KinesisDataStreams")
    ("KinesisDataStreamsParticipant" . "Analytics/KinesisDataStreams")
    ("VPCGroup" . "Groups/VPC")
    ("MigrationHubRefactorSpacesApplications"
     . "MigrationTransfer/MigrationHubRefactorSpacesApplications")
    ("MigrationHubRefactorSpacesApplicationsParticipant"
     . "MigrationTransfer/MigrationHubRefactorSpacesApplications")
    ("RoboMakerFleetManagement" . "Robotics/RoboMakerFleetManagement")
    ("RoboMakerFleetManagementParticipant"
     . "Robotics/RoboMakerFleetManagement")
    ("CostandUsageReport" . "CloudFinancialManagement/CostandUsageReport")
    ("CostandUsageReportParticipant"
     . "CloudFinancialManagement/CostandUsageReport")
    ("BackupAWSBackupsupportforVMwareWorkloads"
     . "Storage/BackupAWSBackupsupportforVMwareWorkloads")
    ("BackupAWSBackupsupportforVMwareWorkloadsParticipant"
     . "Storage/BackupAWSBackupsupportforVMwareWorkloads")
    ("GameKit" . "GameTech/GameKit")
    ("GameKitParticipant" . "GameTech/GameKit")
    ("ManagedBlockchainBlockchain"
     . "Blockchain/ManagedBlockchainBlockchain")
    ("ManagedBlockchainBlockchainParticipant"
     . "Blockchain/ManagedBlockchainBlockchain")
    ("RDSMultiAZDBCluster" . "Database/RDSMultiAZDBCluster")
    ("RDSMultiAZDBClusterParticipant" . "Database/RDSMultiAZDBCluster")
    ("AuroraAmazonRDSInstance" . "Database/AuroraAmazonRDSInstance")
    ("AuroraAmazonRDSInstanceParticipant"
     . "Database/AuroraAmazonRDSInstance")
    ("SavingsPlans" . "CloudFinancialManagement/SavingsPlans")
    ("SavingsPlansParticipant" . "CloudFinancialManagement/SavingsPlans")
    ("ManagedBlockchain" . "Blockchain/ManagedBlockchain")
    ("ManagedBlockchainParticipant" . "Blockchain/ManagedBlockchain")
    ("GenericTurquoiseGroup" . "Groups/GenericTurquoise")
    ("Kinesis" . "Analytics/Kinesis")
    ("KinesisParticipant" . "Analytics/Kinesis")
    ("Robotics" . "Robotics/Robotics")
    ("RoboticsParticipant" . "Robotics/Robotics")
    ("Budgets" . "CloudFinancialManagement/Budgets")
    ("BudgetsParticipant" . "CloudFinancialManagement/Budgets")
    ("SimpleStorageServiceGlacierArchive"
     . "Storage/SimpleStorageServiceGlacierArchive")
    ("SimpleStorageServiceGlacierArchiveParticipant"
     . "Storage/SimpleStorageServiceGlacierArchive")
    ("VPCNetworkAccessControlList"
     . "NetworkingContentDelivery/VPCNetworkAccessControlList")
    ("VPCNetworkAccessControlListParticipant"
     . "NetworkingContentDelivery/VPCNetworkAccessControlList")
    ("QuantumLedgerDatabase" . "Blockchain/QuantumLedgerDatabase")
    ("QuantumLedgerDatabaseParticipant"
     . "Blockchain/QuantumLedgerDatabase")
    ("AuroraInstance" . "Database/AuroraInstance")
    ("AuroraInstanceParticipant" . "Database/AuroraInstance")
    ("Keyspaces" . "Database/Keyspaces")
    ("KeyspacesParticipant" . "Database/Keyspaces")
    ("Kendra" . "MachineLearning/Kendra")
    ("KendraParticipant" . "MachineLearning/Kendra")
    ("Blockchain" . "Blockchain/Blockchain")
    ("BlockchainParticipant" . "Blockchain/Blockchain")
    ("GenericAltGroup" . "Groups/GenericAlt")
    ("LakeFormationDataLake" . "Analytics/LakeFormationDataLake")
    ("LakeFormationDataLakeParticipant"
     . "Analytics/LakeFormationDataLake")
    ("MainframeModernizationRuntime"
     . "MigrationTransfer/MainframeModernizationRuntime")
    ("MainframeModernizationRuntimeParticipant"
     . "MigrationTransfer/MainframeModernizationRuntime")
    ("StorageGatewayFileGateway" . "Storage/StorageGatewayFileGateway")
    ("StorageGatewayFileGatewayParticipant"
     . "Storage/StorageGatewayFileGateway")
    ("BackupComplianceReporting" . "Storage/BackupComplianceReporting")
    ("BackupComplianceReportingParticipant"
     . "Storage/BackupComplianceReporting")
    ("ElasticFileSystemFileSystem"
     . "Storage/ElasticFileSystemFileSystem")
    ("ElasticFileSystemFileSystemParticipant"
     . "Storage/ElasticFileSystemFileSystem")
    ("PrivateLink" . "NetworkingContentDelivery/PrivateLink")
    ("PrivateLinkParticipant" . "NetworkingContentDelivery/PrivateLink")
    ("AuroraMariaDBInstance" . "Database/AuroraMariaDBInstance")
    ("AuroraMariaDBInstanceParticipant"
     . "Database/AuroraMariaDBInstance")
    ("ElastiCacheElastiCacheforMemcached"
     . "Database/ElastiCacheElastiCacheforMemcached")
    ("ElastiCacheElastiCacheforMemcachedParticipant"
     . "Database/ElastiCacheElastiCacheforMemcached")
    ("Lex" . "MachineLearning/Lex")
    ("LexParticipant" . "MachineLearning/Lex")
    ("IoTGreengrassGroup" . "Groups/IoTGreengrass")
    ("VPCCarrierGateway" . "NetworkingContentDelivery/VPCCarrierGateway")
    ("VPCCarrierGatewayParticipant"
     . "NetworkingContentDelivery/VPCCarrierGateway")
    ("KinesisDataAnalytics" . "Analytics/KinesisDataAnalytics")
    ("KinesisDataAnalyticsParticipant" . "Analytics/KinesisDataAnalytics")
    ("ServerMigrationService"
     . "MigrationTransfer/ServerMigrationService")
    ("ServerMigrationServiceParticipant"
     . "MigrationTransfer/ServerMigrationService")
    ("SimpleStorageServiceVPCAccessPoints"
     . "Storage/SimpleStorageServiceVPCAccessPoints")
    ("SimpleStorageServiceVPCAccessPointsParticipant"
     . "Storage/SimpleStorageServiceVPCAccessPoints")
    ("S3onOutposts" . "Storage/S3onOutposts")
    ("S3onOutpostsParticipant" . "Storage/S3onOutposts")
    ("StorageGatewayAmazonFSxFileGateway"
     . "Storage/StorageGatewayAmazonFSxFileGateway")
    ("StorageGatewayAmazonFSxFileGatewayParticipant"
     . "Storage/StorageGatewayAmazonFSxFileGateway")
    ("CloudWAN" . "NetworkingContentDelivery/CloudWAN")
    ("CloudWANParticipant" . "NetworkingContentDelivery/CloudWAN")
    ("ElasticLoadBalancing"
     . "NetworkingContentDelivery/ElasticLoadBalancing")
    ("ElasticLoadBalancingParticipant"
     . "NetworkingContentDelivery/ElasticLoadBalancing")
    ("AuroraMariaDBInstanceAlternate"
     . "Database/AuroraMariaDBInstanceAlternate")
    ("AuroraMariaDBInstanceAlternateParticipant"
     . "Database/AuroraMariaDBInstanceAlternate")
    ("AuroraMySQLInstanceAlternate"
     . "Database/AuroraMySQLInstanceAlternate")
    ("AuroraMySQLInstanceAlternateParticipant"
     . "Database/AuroraMySQLInstanceAlternate")
    ("AWSCloudGroup" . "Groups/AWSCloud")
    ("Rekognition" . "MachineLearning/Rekognition")
    ("RekognitionParticipant" . "MachineLearning/Rekognition")
    ("NetworkingContentDelivery"
     . "NetworkingContentDelivery/NetworkingContentDelivery")
    ("NetworkingContentDeliveryParticipant"
     . "NetworkingContentDelivery/NetworkingContentDelivery")
    ("DataPipeline" . "Analytics/DataPipeline")
    ("DataPipelineParticipant" . "Analytics/DataPipeline")
    ("MainframeModernizationAnalyzer"
     . "MigrationTransfer/MainframeModernizationAnalyzer")
    ("MainframeModernizationAnalyzerParticipant"
     . "MigrationTransfer/MainframeModernizationAnalyzer")
    ("SnowballEdge" . "Storage/SnowballEdge")
    ("SnowballEdgeParticipant" . "Storage/SnowballEdge")
    ("SimpleStorageServiceGlacier"
     . "Storage/SimpleStorageServiceGlacier")
    ("SimpleStorageServiceGlacierParticipant"
     . "Storage/SimpleStorageServiceGlacier")
    ("TransitGateway" . "NetworkingContentDelivery/TransitGateway")
    ("TransitGatewayParticipant"
     . "NetworkingContentDelivery/TransitGateway")
    ("ElasticFileSystemStandard" . "Storage/ElasticFileSystemStandard")
    ("ElasticFileSystemStandardParticipant"
     . "Storage/ElasticFileSystemStandard")
    ("CloudDirectory2" . "NetworkingContentDelivery/CloudDirectory2")
    ("CloudDirectory2Participant"
     . "NetworkingContentDelivery/CloudDirectory2")
    ("DynamoDBStandardAccessTableClass"
     . "Database/DynamoDBStandardAccessTableClass")
    ("DynamoDBStandardAccessTableClassParticipant"
     . "Database/DynamoDBStandardAccessTableClass")
    ("DynamoDBAttributes" . "Database/DynamoDBAttributes")
    ("DynamoDBAttributesParticipant" . "Database/DynamoDBAttributes")
    ("SecurityGroupGroup" . "Groups/SecurityGroup")
    ("Forecast" . "MachineLearning/Forecast")
    ("ForecastParticipant" . "MachineLearning/Forecast")
    ("AppMeshMesh" . "NetworkingContentDelivery/AppMeshMesh")
    ("AppMeshMeshParticipant" . "NetworkingContentDelivery/AppMeshMesh")
    ("TransferFamilyAWSFTP" . "MigrationTransfer/TransferFamilyAWSFTP")
    ("TransferFamilyAWSFTPParticipant"
     . "MigrationTransfer/TransferFamilyAWSFTP")
    ("SimpleStorageServiceS3StandardIA"
     . "Storage/SimpleStorageServiceS3StandardIA")
    ("SimpleStorageServiceS3StandardIAParticipant"
     . "Storage/SimpleStorageServiceS3StandardIA")
    ("CloudEndureDisasterRecovery"
     . "Storage/CloudEndureDisasterRecovery")
    ("CloudEndureDisasterRecoveryParticipant"
     . "Storage/CloudEndureDisasterRecovery")
    ("ElasticLoadBalancingClassicLoadBalancer"
     . "NetworkingContentDelivery/ElasticLoadBalancingClassicLoadBalancer")
    ("ElasticLoadBalancingClassicLoadBalancerParticipant"
     . "NetworkingContentDelivery/ElasticLoadBalancingClassicLoadBalancer")
    ("SnowballSnowballImportExport"
     . "Storage/SnowballSnowballImportExport")
    ("SnowballSnowballImportExportParticipant"
     . "Storage/SnowballSnowballImportExport")
    ("ClientVPN" . "NetworkingContentDelivery/ClientVPN")
    ("ClientVPNParticipant" . "NetworkingContentDelivery/ClientVPN")
    ("DynamoDBItem" . "Database/DynamoDBItem")
    ("DynamoDBItemParticipant" . "Database/DynamoDBItem")
    ("RegionGroup" . "Groups/Region")
    ("RDSProxyInstance" . "Database/RDSProxyInstance")
    ("RDSProxyInstanceParticipant" . "Database/RDSProxyInstance")
    ("DeepLearningAMIs" . "MachineLearning/DeepLearningAMIs")
    ("DeepLearningAMIsParticipant" . "MachineLearning/DeepLearningAMIs")
    ("ElasticLoadBalancingApplicationLoadBalancer"
     . "NetworkingContentDelivery/ElasticLoadBalancingApplicationLoadBalancer")
    ("ElasticLoadBalancingApplicationLoadBalancerParticipant"
     . "NetworkingContentDelivery/ElasticLoadBalancingApplicationLoadBalancer")
    ("KinesisFirehose" . "Analytics/KinesisFirehose")
    ("KinesisFirehoseParticipant" . "Analytics/KinesisFirehose")
    ("MigrationTransfer" . "MigrationTransfer/MigrationTransfer")
    ("MigrationTransferParticipant"
     . "MigrationTransfer/MigrationTransfer")
    ("StorageGatewayVolumeGateway"
     . "Storage/StorageGatewayVolumeGateway")
    ("StorageGatewayVolumeGatewayParticipant"
     . "Storage/StorageGatewayVolumeGateway")
    ("BackupVirtualMachine" . "Storage/BackupVirtualMachine")
    ("BackupVirtualMachineParticipant" . "Storage/BackupVirtualMachine")
    ("CloudFront" . "NetworkingContentDelivery/CloudFront")
    ("CloudFrontParticipant" . "NetworkingContentDelivery/CloudFront")
    ("Storage" . "Storage/Storage")
    ("StorageParticipant" . "Storage/Storage")
    ("VPCVPNConnection" . "NetworkingContentDelivery/VPCVPNConnection")
    ("VPCVPNConnectionParticipant"
     . "NetworkingContentDelivery/VPCVPNConnection")
    ("ElasticBeanstalkContainerGroup"
     . "Groups/ElasticBeanstalkContainer")
    ("DynamoDB" . "Database/DynamoDB")
    ("DynamoDBParticipant" . "Database/DynamoDB")
    ("AuroraOracleInstance" . "Database/AuroraOracleInstance")
    ("AuroraOracleInstanceParticipant" . "Database/AuroraOracleInstance")
    ("Comprehend" . "MachineLearning/Comprehend")
    ("ComprehendParticipant" . "MachineLearning/Comprehend")
    ("AppMeshVirtualNode"
     . "NetworkingContentDelivery/AppMeshVirtualNode")
    ("AppMeshVirtualNodeParticipant"
     . "NetworkingContentDelivery/AppMeshVirtualNode")
    ("FinSpace" . "Analytics/FinSpace")
    ("FinSpaceParticipant" . "Analytics/FinSpace")
    ("TransferFamilyAWSFTPS" . "MigrationTransfer/TransferFamilyAWSFTPS")
    ("TransferFamilyAWSFTPSParticipant"
     . "MigrationTransfer/TransferFamilyAWSFTPS")
    ("SimpleStorageServiceS3ObjectLambda"
     . "Storage/SimpleStorageServiceS3ObjectLambda")
    ("SimpleStorageServiceS3ObjectLambdaParticipant"
     . "Storage/SimpleStorageServiceS3ObjectLambda")
    ("SimpleStorageServiceBucketWithObjects"
     . "Storage/SimpleStorageServiceBucketWithObjects")
    ("SimpleStorageServiceBucketWithObjectsParticipant"
     . "Storage/SimpleStorageServiceBucketWithObjects")
    ("Route53ResolverQueryLogging"
     . "NetworkingContentDelivery/Route53ResolverQueryLogging")
    ("Route53ResolverQueryLoggingParticipant"
     . "NetworkingContentDelivery/Route53ResolverQueryLogging")
    ("Private5G" . "NetworkingContentDelivery/Private5G")
    ("Private5GParticipant" . "NetworkingContentDelivery/Private5G")
    ("BackupAWSBackupsupportforAmazonS3"
     . "Storage/BackupAWSBackupsupportforAmazonS3")
    ("BackupAWSBackupsupportforAmazonS3Participant"
     . "Storage/BackupAWSBackupsupportforAmazonS3")
    ("DatabaseMigrationService" . "Database/DatabaseMigrationService")
    ("DatabaseMigrationServiceParticipant"
     . "Database/DatabaseMigrationService")
    ("RDSProxyInstanceAlternate" . "Database/RDSProxyInstanceAlternate")
    ("RDSProxyInstanceAlternateParticipant"
     . "Database/RDSProxyInstanceAlternate")
    ("VPCNetworkAccessAnalyzer"
     . "NetworkingContentDelivery/VPCNetworkAccessAnalyzer")
    ("VPCNetworkAccessAnalyzerParticipant"
     . "NetworkingContentDelivery/VPCNetworkAccessAnalyzer")
    ("LookoutforVision" . "MachineLearning/LookoutforVision")
    ("LookoutforVisionParticipant" . "MachineLearning/LookoutforVision")
    ("CloudSearch" . "Analytics/CloudSearch")
    ("CloudSearchParticipant" . "Analytics/CloudSearch")
    ("MainframeModernizationConverter"
     . "MigrationTransfer/MainframeModernizationConverter")
    ("MainframeModernizationConverterParticipant"
     . "MigrationTransfer/MainframeModernizationConverter")
    ("SimpleStorageServiceGlacierVault"
     . "Storage/SimpleStorageServiceGlacierVault")
    ("SimpleStorageServiceGlacierVaultParticipant"
     . "Storage/SimpleStorageServiceGlacierVault")
    ("CloudWANVirtualPoP"
     . "NetworkingContentDelivery/CloudWANVirtualPoP")
    ("CloudWANVirtualPoPParticipant"
     . "NetworkingContentDelivery/CloudWANVirtualPoP")
    ("StorageGatewayAmazonS3FileGateway"
     . "Storage/StorageGatewayAmazonS3FileGateway")
    ("StorageGatewayAmazonS3FileGatewayParticipant"
     . "Storage/StorageGatewayAmazonS3FileGateway")
    ("CloudWANCoreNetworkEdge"
     . "NetworkingContentDelivery/CloudWANCoreNetworkEdge")
    ("CloudWANCoreNetworkEdgeParticipant"
     . "NetworkingContentDelivery/CloudWANCoreNetworkEdge")
    ("SimpleStorageServiceS3GlacierDeepArchive"
     . "Storage/SimpleStorageServiceS3GlacierDeepArchive")
    ("SimpleStorageServiceS3GlacierDeepArchiveParticipant"
     . "Storage/SimpleStorageServiceS3GlacierDeepArchive")
    ("ElastiCache" . "Database/ElastiCache")
    ("ElastiCacheParticipant" . "Database/ElastiCache")
    ("GenericPurpleGroup" . "Groups/GenericPurple")
    ("CloudFrontFunctions"
     . "NetworkingContentDelivery/CloudFrontFunctions")
    ("CloudFrontFunctionsParticipant"
     . "NetworkingContentDelivery/CloudFrontFunctions")
    ("Monitron" . "MachineLearning/Monitron")
    ("MonitronParticipant" . "MachineLearning/Monitron")
    ("Glue" . "Analytics/Glue") ("GlueParticipant" . "Analytics/Glue")
    ("ApplicationMigrationService"
     . "MigrationTransfer/ApplicationMigrationService")
    ("ApplicationMigrationServiceParticipant"
     . "MigrationTransfer/ApplicationMigrationService")
    ("SimpleStorageServiceS3Replication"
     . "Storage/SimpleStorageServiceS3Replication")
    ("SimpleStorageServiceS3ReplicationParticipant"
     . "Storage/SimpleStorageServiceS3Replication")
    ("CloudFrontEdgeLocation"
     . "NetworkingContentDelivery/CloudFrontEdgeLocation")
    ("CloudFrontEdgeLocationParticipant"
     . "NetworkingContentDelivery/CloudFrontEdgeLocation")
    ("ElasticFileSystemOneZone" . "Storage/ElasticFileSystemOneZone")
    ("ElasticFileSystemOneZoneParticipant"
     . "Storage/ElasticFileSystemOneZone")
    ("GlobalAccelerator" . "NetworkingContentDelivery/GlobalAccelerator")
    ("GlobalAcceleratorParticipant"
     . "NetworkingContentDelivery/GlobalAccelerator")
    ("StorageGatewayTapeGateway" . "Storage/StorageGatewayTapeGateway")
    ("StorageGatewayTapeGatewayParticipant"
     . "Storage/StorageGatewayTapeGateway")
    ("Timestream" . "Database/Timestream")
    ("TimestreamParticipant" . "Database/Timestream")
    ("AuroraAmazonAuroraInstancealternate"
     . "Database/AuroraAmazonAuroraInstancealternate")
    ("AuroraAmazonAuroraInstancealternateParticipant"
     . "Database/AuroraAmazonAuroraInstancealternate")
    ("StepFunctionsWorkflowGroup" . "Groups/StepFunctionsWorkflow")
    ("Route53Resolver" . "NetworkingContentDelivery/Route53Resolver")
    ("Route53ResolverParticipant"
     . "NetworkingContentDelivery/Route53Resolver")
    ("DeepLearningContainers" . "MachineLearning/DeepLearningContainers")
    ("DeepLearningContainersParticipant"
     . "MachineLearning/DeepLearningContainers")
    ("LakeFormation" . "Analytics/LakeFormation")
    ("LakeFormationParticipant" . "Analytics/LakeFormation")
    ("MainframeModernization"
     . "MigrationTransfer/MainframeModernization")
    ("MainframeModernizationParticipant"
     . "MigrationTransfer/MainframeModernization")
    ("ElasticFileSystemIntelligentTiering"
     . "Storage/ElasticFileSystemIntelligentTiering")
    ("ElasticFileSystemIntelligentTieringParticipant"
     . "Storage/ElasticFileSystemIntelligentTiering")
    ("Route53ResolverDNSFirewall"
     . "NetworkingContentDelivery/Route53ResolverDNSFirewall")
    ("Route53ResolverDNSFirewallParticipant"
     . "NetworkingContentDelivery/Route53ResolverDNSFirewall")
    ("SimpleStorageServiceS3ReplicationTimeControl"
     . "Storage/SimpleStorageServiceS3ReplicationTimeControl")
    ("SimpleStorageServiceS3ReplicationTimeControlParticipant"
     . "Storage/SimpleStorageServiceS3ReplicationTimeControl")
    ("VPCVPNGateway" . "NetworkingContentDelivery/VPCVPNGateway")
    ("VPCVPNGatewayParticipant"
     . "NetworkingContentDelivery/VPCVPNGateway")
    ("QuantumLedgerDatabase2" . "Database/QuantumLedgerDatabase2")
    ("QuantumLedgerDatabase2Participant"
     . "Database/QuantumLedgerDatabase2")
    ("DynamoDBAttribute" . "Database/DynamoDBAttribute")
    ("DynamoDBAttributeParticipant" . "Database/DynamoDBAttribute")
    ("GenericOrangeGroup" . "Groups/GenericOrange")
    ("SitetoSiteVPN" . "NetworkingContentDelivery/SitetoSiteVPN")
    ("SitetoSiteVPNParticipant"
     . "NetworkingContentDelivery/SitetoSiteVPN")
    ("HealthLake" . "MachineLearning/HealthLake")
    ("HealthLakeParticipant" . "MachineLearning/HealthLake")
    ("MSKAmazonMSKConnect" . "Analytics/MSKAmazonMSKConnect")
    ("MSKAmazonMSKConnectParticipant" . "Analytics/MSKAmazonMSKConnect")
    ("DatasyncAgent" . "MigrationTransfer/DatasyncAgent")
    ("DatasyncAgentParticipant" . "MigrationTransfer/DatasyncAgent")
    ("ElasticBlockStoreMultipleVolumes"
     . "Storage/ElasticBlockStoreMultipleVolumes")
    ("ElasticBlockStoreMultipleVolumesParticipant"
     . "Storage/ElasticBlockStoreMultipleVolumes")
    ("VPCCustomerGateway"
     . "NetworkingContentDelivery/VPCCustomerGateway")
    ("VPCCustomerGatewayParticipant"
     . "NetworkingContentDelivery/VPCCustomerGateway")
    ("BackupBackupRestore" . "Storage/BackupBackupRestore")
    ("BackupBackupRestoreParticipant" . "Storage/BackupBackupRestore")
    ("AppMeshVirtualGateway"
     . "NetworkingContentDelivery/AppMeshVirtualGateway")
    ("AppMeshVirtualGatewayParticipant"
     . "NetworkingContentDelivery/AppMeshVirtualGateway")
    ("FSxforLustre" . "Storage/FSxforLustre")
    ("FSxforLustreParticipant" . "Storage/FSxforLustre")
    ("DocumentDB" . "Database/DocumentDB")
    ("DocumentDBParticipant" . "Database/DocumentDB")
    ("ElastiCacheCacheNode" . "Database/ElastiCacheCacheNode")
    ("ElastiCacheCacheNodeParticipant" . "Database/ElastiCacheCacheNode")
    ("GenericGreenGroup" . "Groups/GenericGreen")
    ("VPCNATGateway" . "NetworkingContentDelivery/VPCNATGateway")
    ("VPCNATGatewayParticipant"
     . "NetworkingContentDelivery/VPCNATGateway")
    ("ApacheMXNetonAWS" . "MachineLearning/ApacheMXNetonAWS")
    ("ApacheMXNetonAWSParticipant" . "MachineLearning/ApacheMXNetonAWS")
    ("MigrationHubRefactorSpacesServices"
     . "MigrationTransfer/MigrationHubRefactorSpacesServices")
    ("MigrationHubRefactorSpacesServicesParticipant"
     . "MigrationTransfer/MigrationHubRefactorSpacesServices")
    ("EMRHDFSCluster" . "Analytics/EMRHDFSCluster")
    ("EMRHDFSClusterParticipant" . "Analytics/EMRHDFSCluster")
    ("Backup" . "Storage/Backup")
    ("BackupParticipant" . "Storage/Backup")
    ("CloudMapService" . "NetworkingContentDelivery/CloudMapService")
    ("CloudMapServiceParticipant"
     . "NetworkingContentDelivery/CloudMapService")
    ("ElasticFileSystemOneZoneInfrequentAccess"
     . "Storage/ElasticFileSystemOneZoneInfrequentAccess")
    ("ElasticFileSystemOneZoneInfrequentAccessParticipant"
     . "Storage/ElasticFileSystemOneZoneInfrequentAccess")
    ("ElasticLoadBalancingNetworkLoadBalancer"
     . "NetworkingContentDelivery/ElasticLoadBalancingNetworkLoadBalancer")
    ("ElasticLoadBalancingNetworkLoadBalancerParticipant"
     . "NetworkingContentDelivery/ElasticLoadBalancingNetworkLoadBalancer")
    ("FSxforWFS" . "Storage/FSxforWFS")
    ("FSxforWFSParticipant" . "Storage/FSxforWFS")
    ("AuroraMySQLInstance" . "Database/AuroraMySQLInstance")
    ("AuroraMySQLInstanceParticipant" . "Database/AuroraMySQLInstance")
    ("RDSonVMware" . "Database/RDSonVMware")
    ("RDSonVMwareParticipant" . "Database/RDSonVMware")
    ("SpotFleetGroup" . "Groups/SpotFleet")
    ("AppMeshVirtualRouter"
     . "NetworkingContentDelivery/AppMeshVirtualRouter")
    ("AppMeshVirtualRouterParticipant"
     . "NetworkingContentDelivery/AppMeshVirtualRouter")
    ("MigrationHubRefactorSpacesEnvironments"
     . "MigrationTransfer/MigrationHubRefactorSpacesEnvironments")
    ("MigrationHubRefactorSpacesEnvironmentsParticipant"
     . "MigrationTransfer/MigrationHubRefactorSpacesEnvironments")
    ("SimpleStorageServiceS3ObjectLambdaAccessPoints"
     . "Storage/SimpleStorageServiceS3ObjectLambdaAccessPoints")
    ("SimpleStorageServiceS3ObjectLambdaAccessPointsParticipant"
     . "Storage/SimpleStorageServiceS3ObjectLambdaAccessPoints")
    ("EMR" . "Analytics/EMR") ("EMRParticipant" . "Analytics/EMR")
    ("SimpleStorageServiceS3GlacierFlexibleRetrieval"
     . "Storage/SimpleStorageServiceS3GlacierFlexibleRetrieval")
    ("SimpleStorageServiceS3GlacierFlexibleRetrievalParticipant"
     . "Storage/SimpleStorageServiceS3GlacierFlexibleRetrieval")
    ("VPCInternetGateway"
     . "NetworkingContentDelivery/VPCInternetGateway")
    ("VPCInternetGatewayParticipant"
     . "NetworkingContentDelivery/VPCInternetGateway")
    ("Aurora" . "Database/Aurora")
    ("AuroraParticipant" . "Database/Aurora")
    ("SimpleStorageServiceS3OnOutposts"
     . "Storage/SimpleStorageServiceS3OnOutposts")
    ("SimpleStorageServiceS3OnOutpostsParticipant"
     . "Storage/SimpleStorageServiceS3OnOutposts")
    ("IoTGreengrassDeploymentGroup" . "Groups/IoTGreengrassDeployment")
    ("DynamoDBGlobalsecondaryindex"
     . "Database/DynamoDBGlobalsecondaryindex")
    ("DynamoDBGlobalsecondaryindexParticipant"
     . "Database/DynamoDBGlobalsecondaryindex")
    ("ElasticLoadBalancingGatewayLoadBalancer"
     . "NetworkingContentDelivery/ElasticLoadBalancingGatewayLoadBalancer")
    ("ElasticLoadBalancingGatewayLoadBalancerParticipant"
     . "NetworkingContentDelivery/ElasticLoadBalancingGatewayLoadBalancer")
    ("Textract" . "MachineLearning/Textract")
    ("TextractParticipant" . "MachineLearning/Textract")
    ("DataSync" . "MigrationTransfer/DataSync")
    ("DataSyncParticipant" . "MigrationTransfer/DataSync")
    ("BackupRecoveryTimeObjective"
     . "Storage/BackupRecoveryTimeObjective")
    ("BackupRecoveryTimeObjectiveParticipant"
     . "Storage/BackupRecoveryTimeObjective")
    ("EMREMREngine" . "Analytics/EMREMREngine")
    ("EMREMREngineParticipant" . "Analytics/EMREMREngine")
    ("CloudMapNamespace" . "NetworkingContentDelivery/CloudMapNamespace")
    ("CloudMapNamespaceParticipant"
     . "NetworkingContentDelivery/CloudMapNamespace")
    ("CloudFrontStreamingDistribution"
     . "NetworkingContentDelivery/CloudFrontStreamingDistribution")
    ("CloudFrontStreamingDistributionParticipant"
     . "NetworkingContentDelivery/CloudFrontStreamingDistribution")
    ("StorageGatewayVirtualTapeLibrary"
     . "Storage/StorageGatewayVirtualTapeLibrary")
    ("StorageGatewayVirtualTapeLibraryParticipant"
     . "Storage/StorageGatewayVirtualTapeLibrary")
    ("MemoryDBforRedis" . "Database/MemoryDBforRedis")
    ("MemoryDBforRedisParticipant" . "Database/MemoryDBforRedis")
    ("FSxforOpenZFS" . "Storage/FSxforOpenZFS")
    ("FSxforOpenZFSParticipant" . "Storage/FSxforOpenZFS")
    ("GenericBlueGroup" . "Groups/GenericBlue")
    ("DatabaseMigrationServiceDatabasemigrationworkflowjob"
     . "Database/DatabaseMigrationServiceDatabasemigrationworkflowjob")
    ("DatabaseMigrationServiceDatabasemigrationworkflowjobParticipant"
     . "Database/DatabaseMigrationServiceDatabasemigrationworkflowjob")
    ("SageMakerModel" . "MachineLearning/SageMakerModel")
    ("SageMakerModelParticipant" . "MachineLearning/SageMakerModel")
    ("VPCTrafficMirroring"
     . "NetworkingContentDelivery/VPCTrafficMirroring")
    ("VPCTrafficMirroringParticipant"
     . "NetworkingContentDelivery/VPCTrafficMirroring")
    ("MainframeModernizationDeveloper"
     . "MigrationTransfer/MainframeModernizationDeveloper")
    ("MainframeModernizationDeveloperParticipant"
     . "MigrationTransfer/MainframeModernizationDeveloper")
    ("Snowcone" . "Storage/Snowcone")
    ("SnowconeParticipant" . "Storage/Snowcone")
    ("CloudWANSegmentNetwork"
     . "NetworkingContentDelivery/CloudWANSegmentNetwork")
    ("CloudWANSegmentNetworkParticipant"
     . "NetworkingContentDelivery/CloudWANSegmentNetwork")
    ("KinesisVideoStreams" . "Analytics/KinesisVideoStreams")
    ("KinesisVideoStreamsParticipant" . "Analytics/KinesisVideoStreams")
    ("Route53HostedZone" . "NetworkingContentDelivery/Route53HostedZone")
    ("Route53HostedZoneParticipant"
     . "NetworkingContentDelivery/Route53HostedZone")
    ("SimpleStorageServiceS3OneZoneIA"
     . "Storage/SimpleStorageServiceS3OneZoneIA")
    ("SimpleStorageServiceS3OneZoneIAParticipant"
     . "Storage/SimpleStorageServiceS3OneZoneIA")
    ("ElasticBlockStoreSnapshot" . "Storage/ElasticBlockStoreSnapshot")
    ("ElasticBlockStoreSnapshotParticipant"
     . "Storage/ElasticBlockStoreSnapshot")
    ("Database" . "Database/Database")
    ("DatabaseParticipant" . "Database/Database")
    ("EC2InstanceContentsGroup" . "Groups/EC2InstanceContents")
    ("AuroraOracleInstanceAlternate"
     . "Database/AuroraOracleInstanceAlternate")
    ("AuroraOracleInstanceAlternateParticipant"
     . "Database/AuroraOracleInstanceAlternate")
    ("Transcribe" . "MachineLearning/Transcribe")
    ("TranscribeParticipant" . "MachineLearning/Transcribe")
    ("VPCFlowLogs" . "NetworkingContentDelivery/VPCFlowLogs")
    ("VPCFlowLogsParticipant" . "NetworkingContentDelivery/VPCFlowLogs")
    ("ApplicationDiscoveryService"
     . "MigrationTransfer/ApplicationDiscoveryService")
    ("ApplicationDiscoveryServiceParticipant"
     . "MigrationTransfer/ApplicationDiscoveryService")
    ("FSx" . "Storage/FSx") ("FSxParticipant" . "Storage/FSx")
    ("GlueCrawler" . "Analytics/GlueCrawler")
    ("GlueCrawlerParticipant" . "Analytics/GlueCrawler")
    ("TransitGatewayAttachment"
     . "NetworkingContentDelivery/TransitGatewayAttachment")
    ("TransitGatewayAttachmentParticipant"
     . "NetworkingContentDelivery/TransitGatewayAttachment")
    ("CloudFrontDownloadDistribution"
     . "NetworkingContentDelivery/CloudFrontDownloadDistribution")
    ("CloudFrontDownloadDistributionParticipant"
     . "NetworkingContentDelivery/CloudFrontDownloadDistribution")
    ("EFS" . "Storage/EFS") ("EFSParticipant" . "Storage/EFS")
    ("SimpleStorageServiceS3StorageLens"
     . "Storage/SimpleStorageServiceS3StorageLens")
    ("SimpleStorageServiceS3StorageLensParticipant"
     . "Storage/SimpleStorageServiceS3StorageLens")
    ("Neptune" . "Database/Neptune")
    ("NeptuneParticipant" . "Database/Neptune")
    ("AuroraSQLServerInstance" . "Database/AuroraSQLServerInstance")
    ("AuroraSQLServerInstanceParticipant"
     . "Database/AuroraSQLServerInstance")
    ("ServerContentsGroup" . "Groups/ServerContents")
    ("CodeGuru" . "MachineLearning/CodeGuru")
    ("CodeGuruParticipant" . "MachineLearning/CodeGuru")
    ("Route53RouteTable" . "NetworkingContentDelivery/Route53RouteTable")
    ("Route53RouteTableParticipant"
     . "NetworkingContentDelivery/Route53RouteTable")
    ("SimpleStorageServiceObject" . "Storage/SimpleStorageServiceObject")
    ("SimpleStorageServiceObjectParticipant"
     . "Storage/SimpleStorageServiceObject")
    ("Email" . "General/Email") ("EmailParticipant" . "General/Email")
    ("GlueDataBrew" . "Analytics/GlueDataBrew")
    ("GlueDataBrewParticipant" . "Analytics/GlueDataBrew")
    ("CloudMap" . "NetworkingContentDelivery/CloudMap")
    ("CloudMapParticipant" . "NetworkingContentDelivery/CloudMap")
    ("AppMeshVirtualService"
     . "NetworkingContentDelivery/AppMeshVirtualService")
    ("AppMeshVirtualServiceParticipant"
     . "NetworkingContentDelivery/AppMeshVirtualService")
    ("ElasticBlockStoreAmazonDataLifecycleManager"
     . "Storage/ElasticBlockStoreAmazonDataLifecycleManager")
    ("ElasticBlockStoreAmazonDataLifecycleManagerParticipant"
     . "Storage/ElasticBlockStoreAmazonDataLifecycleManager")
    ("AuroraPostgreSQLInstance" . "Database/AuroraPostgreSQLInstance")
    ("AuroraPostgreSQLInstanceParticipant"
     . "Database/AuroraPostgreSQLInstance")
    ("SimpleStorageServiceS3Standard"
     . "Storage/SimpleStorageServiceS3Standard")
    ("SimpleStorageServiceS3StandardParticipant"
     . "Storage/SimpleStorageServiceS3Standard")
    ("AWSAccountGroup" . "Groups/AWSAccount")
    ("DynamoDBItems" . "Database/DynamoDBItems")
    ("DynamoDBItemsParticipant" . "Database/DynamoDBItems")
    ("DevOpsGuru" . "MachineLearning/DevOpsGuru")
    ("DevOpsGuruParticipant" . "MachineLearning/DevOpsGuru")
    ("VPCReachabilityAnalyzer"
     . "NetworkingContentDelivery/VPCReachabilityAnalyzer")
    ("VPCReachabilityAnalyzerParticipant"
     . "NetworkingContentDelivery/VPCReachabilityAnalyzer")
    ("SimpleStorageServiceBucket" . "Storage/SimpleStorageServiceBucket")
    ("SimpleStorageServiceBucketParticipant"
     . "Storage/SimpleStorageServiceBucket")
    ("Recover" . "General/Recover")
    ("RecoverParticipant" . "General/Recover")
    ("OpenSearchService" . "Analytics/OpenSearchService")
    ("OpenSearchServiceParticipant" . "Analytics/OpenSearchService")
    ("ChimeVoiceConnector" . "BusinessApplications/ChimeVoiceConnector")
    ("ChimeVoiceConnectorParticipant"
     . "BusinessApplications/ChimeVoiceConnector")
    ("BusinessApplications" . "BusinessApplications/BusinessApplications")
    ("BusinessApplicationsParticipant"
     . "BusinessApplications/BusinessApplications")
    ("BackupDatabase" . "Storage/BackupDatabase")
    ("BackupDatabaseParticipant" . "Storage/BackupDatabase")
    ("VPCPeeringConnection"
     . "NetworkingContentDelivery/VPCPeeringConnection")
    ("VPCPeeringConnectionParticipant"
     . "NetworkingContentDelivery/VPCPeeringConnection")
    ("CorporateDataCenterGroup" . "Groups/CorporateDataCenter")
    ("BackupStorage" . "Storage/BackupStorage")
    ("BackupStorageParticipant" . "Storage/BackupStorage")
    ("DirectConnect" . "NetworkingContentDelivery/DirectConnect")
    ("DirectConnectParticipant"
     . "NetworkingContentDelivery/DirectConnect")
    ("AugmentedAIA2I" . "MachineLearning/AugmentedAIA2I")
    ("AugmentedAIA2IParticipant" . "MachineLearning/AugmentedAIA2I")
    ("VPCElasticNetworkAdapter"
     . "NetworkingContentDelivery/VPCElasticNetworkAdapter")
    ("VPCElasticNetworkAdapterParticipant"
     . "NetworkingContentDelivery/VPCElasticNetworkAdapter")
    ("BackupVirtualMachineMonitor"
     . "Storage/BackupVirtualMachineMonitor")
    ("BackupVirtualMachineMonitorParticipant"
     . "Storage/BackupVirtualMachineMonitor")
    ("Internet" . "General/Internet")
    ("InternetParticipant" . "General/Internet")
    ("RedshiftDenseComputeNode" . "Analytics/RedshiftDenseComputeNode")
    ("RedshiftDenseComputeNodeParticipant"
     . "Analytics/RedshiftDenseComputeNode")
    ("SimpleEmailService" . "BusinessApplications/SimpleEmailService")
    ("SimpleEmailServiceParticipant"
     . "BusinessApplications/SimpleEmailService")
    ("Pinpoint" . "BusinessApplications/Pinpoint")
    ("PinpointParticipant" . "BusinessApplications/Pinpoint")
    ("SimpleStorageServiceGeneralAccessPoints"
     . "Storage/SimpleStorageServiceGeneralAccessPoints")
    ("SimpleStorageServiceGeneralAccessPointsParticipant"
     . "Storage/SimpleStorageServiceGeneralAccessPoints")
    ("Route53" . "NetworkingContentDelivery/Route53")
    ("Route53Participant" . "NetworkingContentDelivery/Route53")
    ("BackupBackupPlan" . "Storage/BackupBackupPlan")
    ("BackupBackupPlanParticipant" . "Storage/BackupBackupPlan")
    ("PublicSubnetGroup" . "Groups/PublicSubnet")
    ("VPCRouter" . "NetworkingContentDelivery/VPCRouter")
    ("VPCRouterParticipant" . "NetworkingContentDelivery/VPCRouter")
    ("DirectConnectGateway"
     . "NetworkingContentDelivery/DirectConnectGateway")
    ("DirectConnectGatewayParticipant"
     . "NetworkingContentDelivery/DirectConnectGateway")
    ("SageMakerGroundTruth" . "MachineLearning/SageMakerGroundTruth")
    ("SageMakerGroundTruthParticipant"
     . "MachineLearning/SageMakerGroundTruth")
    ("Snowmobile" . "Storage/Snowmobile")
    ("SnowmobileParticipant" . "Storage/Snowmobile")
    ("Internetalt2" . "General/Internetalt2")
    ("Internetalt2Participant" . "General/Internetalt2")
    ("ManagedStreamingforApacheKafka"
     . "Analytics/ManagedStreamingforApacheKafka")
    ("ManagedStreamingforApacheKafkaParticipant"
     . "Analytics/ManagedStreamingforApacheKafka")
    ("ServiceCatalog" . "ManagementGovernance/ServiceCatalog")
    ("ServiceCatalogParticipant" . "ManagementGovernance/ServiceCatalog")
    ("ElasticBlockStoreVolume" . "Storage/ElasticBlockStoreVolume")
    ("ElasticBlockStoreVolumeParticipant"
     . "Storage/ElasticBlockStoreVolume")
    ("AWSCloudAltGroup" . "Groups/AWSCloudAlt")
    ("SimpleStorageServiceS3GlacierInstantRetrieval"
     . "Storage/SimpleStorageServiceS3GlacierInstantRetrieval")
    ("SimpleStorageServiceS3GlacierInstantRetrievalParticipant"
     . "Storage/SimpleStorageServiceS3GlacierInstantRetrieval")
    ("Route53ApplicationRecoveryController"
     . "NetworkingContentDelivery/Route53ApplicationRecoveryController")
    ("Route53ApplicationRecoveryControllerParticipant"
     . "NetworkingContentDelivery/Route53ApplicationRecoveryController")
    ("VirtualPrivateCloud"
     . "NetworkingContentDelivery/VirtualPrivateCloud")
    ("VirtualPrivateCloudParticipant"
     . "NetworkingContentDelivery/VirtualPrivateCloud")
    ("AppMesh" . "NetworkingContentDelivery/AppMesh")
    ("AppMeshParticipant" . "NetworkingContentDelivery/AppMesh")
    ("SageMakerTrain" . "MachineLearning/SageMakerTrain")
    ("SageMakerTrainParticipant" . "MachineLearning/SageMakerTrain")
    ("ElasticBlockStore" . "Storage/ElasticBlockStore")
    ("ElasticBlockStoreParticipant" . "Storage/ElasticBlockStore")
    ("Disk" . "General/Disk") ("DiskParticipant" . "General/Disk")
    ("DataExchangeforAPIs" . "Analytics/DataExchangeforAPIs")
    ("DataExchangeforAPIsParticipant" . "Analytics/DataExchangeforAPIs")
    ("SimpleEmailServiceEmail"
     . "BusinessApplications/SimpleEmailServiceEmail")
    ("SimpleEmailServiceEmailParticipant"
     . "BusinessApplications/SimpleEmailServiceEmail")
    ("SystemsManagerMaintenanceWindows"
     . "ManagementGovernance/SystemsManagerMaintenanceWindows")
    ("SystemsManagerMaintenanceWindowsParticipant"
     . "ManagementGovernance/SystemsManagerMaintenanceWindows")
    ("PrivateSubnetGroup" . "Groups/PrivateSubnet")
    ("BackupRecoveryPointObjective"
     . "Storage/BackupRecoveryPointObjective")
    ("BackupRecoveryPointObjectiveParticipant"
     . "Storage/BackupRecoveryPointObjective")
    ("Snowball" . "Storage/Snowball")
    ("SnowballParticipant" . "Storage/Snowball")
    ("CloudMapResource" . "NetworkingContentDelivery/CloudMapResource")
    ("CloudMapResourceParticipant"
     . "NetworkingContentDelivery/CloudMapResource")
    ("VPCEndpoints" . "NetworkingContentDelivery/VPCEndpoints")
    ("VPCEndpointsParticipant" . "NetworkingContentDelivery/VPCEndpoints")
    ("VPCElasticNetworkInterface"
     . "NetworkingContentDelivery/VPCElasticNetworkInterface")
    ("VPCElasticNetworkInterfaceParticipant"
     . "NetworkingContentDelivery/VPCElasticNetworkInterface")
    ("RekognitionVideo" . "MachineLearning/RekognitionVideo")
    ("RekognitionVideoParticipant" . "MachineLearning/RekognitionVideo")
    ("BackupCompute" . "Storage/BackupCompute")
    ("BackupComputeParticipant" . "Storage/BackupCompute")
    ("SAMLtoken" . "General/SAMLtoken")
    ("SAMLtokenParticipant" . "General/SAMLtoken")
    ("GlueDataCatalog" . "Analytics/GlueDataCatalog")
    ("GlueDataCatalogParticipant" . "Analytics/GlueDataCatalog")
    ("WorkDocs" . "BusinessApplications/WorkDocs")
    ("WorkDocsParticipant" . "BusinessApplications/WorkDocs")
    ("GenericPinkGroup" . "Groups/GenericPink")
    ("TrustedAdvisorChecklist"
     . "ManagementGovernance/TrustedAdvisorChecklist")
    ("TrustedAdvisorChecklistParticipant"
     . "ManagementGovernance/TrustedAdvisorChecklist")
    ("BackupAWSBackupsupportforAmazonFSxforNetAppONTAP"
     . "Storage/BackupAWSBackupsupportforAmazonFSxforNetAppONTAP")
    ("BackupAWSBackupsupportforAmazonFSxforNetAppONTAPParticipant"
     . "Storage/BackupAWSBackupsupportforAmazonFSxforNetAppONTAP")
    ("Route53RoutingControls"
     . "NetworkingContentDelivery/Route53RoutingControls")
    ("Route53RoutingControlsParticipant"
     . "NetworkingContentDelivery/Route53RoutingControls")
    ("Route53ReadinessChecks"
     . "NetworkingContentDelivery/Route53ReadinessChecks")
    ("Route53ReadinessChecksParticipant"
     . "NetworkingContentDelivery/Route53ReadinessChecks")
    ("CloudWatchLogs" . "ManagementGovernance/CloudWatchLogs")
    ("CloudWatchLogsParticipant" . "ManagementGovernance/CloudWatchLogs")
    ("Chatbot" . "ManagementGovernance/Chatbot")
    ("ChatbotParticipant" . "ManagementGovernance/Chatbot")
    ("LookoutforMetrics" . "MachineLearning/LookoutforMetrics")
    ("LookoutforMetricsParticipant" . "MachineLearning/LookoutforMetrics")
    ("StorageGatewayCachedVolume" . "Storage/StorageGatewayCachedVolume")
    ("StorageGatewayCachedVolumeParticipant"
     . "Storage/StorageGatewayCachedVolume")
    ("GitRepository" . "General/GitRepository")
    ("GitRepositoryParticipant" . "General/GitRepository")
    ("Athena" . "Analytics/Athena")
    ("AthenaParticipant" . "Analytics/Athena")
    ("AvailabilityZoneGroup" . "Groups/AvailabilityZone")
    ("Chime" . "BusinessApplications/Chime")
    ("ChimeParticipant" . "BusinessApplications/Chime")
    ("CloudWatchMetricsInsights"
     . "ManagementGovernance/CloudWatchMetricsInsights")
    ("CloudWatchMetricsInsightsParticipant"
     . "ManagementGovernance/CloudWatchMetricsInsights")
    ("KinesisVideoStreams2" . "MediaServices/KinesisVideoStreams2")
    ("KinesisVideoStreams2Participant"
     . "MediaServices/KinesisVideoStreams2")
    ("CloudWatchRule" . "ManagementGovernance/CloudWatchRule")
    ("CloudWatchRuleParticipant" . "ManagementGovernance/CloudWatchRule")
    ("ApplicationAutoScaling2"
     . "ManagementGovernance/ApplicationAutoScaling2")
    ("ApplicationAutoScaling2Participant"
     . "ManagementGovernance/ApplicationAutoScaling2")
    ("CloudWatchEventTimeBased"
     . "ManagementGovernance/CloudWatchEventTimeBased")
    ("CloudWatchEventTimeBasedParticipant"
     . "ManagementGovernance/CloudWatchEventTimeBased")
    ("CloudWatchRUM" . "ManagementGovernance/CloudWatchRUM")
    ("CloudWatchRUMParticipant" . "ManagementGovernance/CloudWatchRUM")
    ("Neuron" . "MachineLearning/Neuron")
    ("NeuronParticipant" . "MachineLearning/Neuron")
    ("SimpleStorageService" . "Storage/SimpleStorageService")
    ("SimpleStorageServiceParticipant" . "Storage/SimpleStorageService")
    ("GenericApplication" . "General/GenericApplication")
    ("GenericApplicationParticipant" . "General/GenericApplication")
    ("GenericRedGroup" . "Groups/GenericRed")
    ("PinpointAPIs" . "BusinessApplications/PinpointAPIs")
    ("PinpointAPIsParticipant" . "BusinessApplications/PinpointAPIs")
    ("RedshiftML" . "Analytics/RedshiftML")
    ("RedshiftMLParticipant" . "Analytics/RedshiftML")
    ("OpsWorksInstances" . "ManagementGovernance/OpsWorksInstances")
    ("OpsWorksInstancesParticipant"
     . "ManagementGovernance/OpsWorksInstances")
    ("MediaServices" . "MediaServices/MediaServices")
    ("MediaServicesParticipant" . "MediaServices/MediaServices")
    ("ElasticContainerServiceECSAnywhere"
     . "Containers/ElasticContainerServiceECSAnywhere")
    ("ElasticContainerServiceECSAnywhereParticipant"
     . "Containers/ElasticContainerServiceECSAnywhere")
    ("OrganizationsManagementAccount"
     . "ManagementGovernance/OrganizationsManagementAccount")
    ("OrganizationsManagementAccountParticipant"
     . "ManagementGovernance/OrganizationsManagementAccount")
    ("CloudFormationChangeSet"
     . "ManagementGovernance/CloudFormationChangeSet")
    ("CloudFormationChangeSetParticipant"
     . "ManagementGovernance/CloudFormationChangeSet")
    ("SystemsManagerDocuments"
     . "ManagementGovernance/SystemsManagerDocuments")
    ("SystemsManagerDocumentsParticipant"
     . "ManagementGovernance/SystemsManagerDocuments")
    ("DeepLens" . "MachineLearning/DeepLens")
    ("DeepLensParticipant" . "MachineLearning/DeepLens")
    ("ElasticFileSystemStandardInfrequentAccess"
     . "Storage/ElasticFileSystemStandardInfrequentAccess")
    ("ElasticFileSystemStandardInfrequentAccessParticipant"
     . "Storage/ElasticFileSystemStandardInfrequentAccess")
    ("AutoScalingGroupGroup" . "Groups/AutoScalingGroup")
    ("Client" . "General/Client")
    ("ClientParticipant" . "General/Client")
    ("AlexaForBusiness" . "BusinessApplications/AlexaForBusiness")
    ("AlexaForBusinessParticipant"
     . "BusinessApplications/AlexaForBusiness")
    ("DataExchange" . "Analytics/DataExchange")
    ("DataExchangeParticipant" . "Analytics/DataExchange")
    ("TrustedAdvisor" . "ManagementGovernance/TrustedAdvisor")
    ("TrustedAdvisorParticipant" . "ManagementGovernance/TrustedAdvisor")
    ("ElasticTranscoder" . "MediaServices/ElasticTranscoder")
    ("ElasticTranscoderParticipant" . "MediaServices/ElasticTranscoder")
    ("ElasticContainerRegistry" . "Containers/ElasticContainerRegistry")
    ("ElasticContainerRegistryParticipant"
     . "Containers/ElasticContainerRegistry")
    ("SystemsManager" . "ManagementGovernance/SystemsManager")
    ("SystemsManagerParticipant" . "ManagementGovernance/SystemsManager")
    ("SystemsManagerIncidentManager"
     . "ManagementGovernance/SystemsManagerIncidentManager")
    ("SystemsManagerIncidentManagerParticipant"
     . "ManagementGovernance/SystemsManagerIncidentManager")
    ("CloudWatch" . "ManagementGovernance/CloudWatch")
    ("CloudWatchParticipant" . "ManagementGovernance/CloudWatch")
    ("SageMakerStudioLab" . "MachineLearning/SageMakerStudioLab")
    ("SageMakerStudioLabParticipant"
     . "MachineLearning/SageMakerStudioLab")
    ("GenericGroup" . "Groups/Generic")
    ("StorageGatewayNoncachedVolume"
     . "Storage/StorageGatewayNoncachedVolume")
    ("StorageGatewayNoncachedVolumeParticipant"
     . "Storage/StorageGatewayNoncachedVolume")
    ("MagnifyingGlass" . "General/MagnifyingGlass")
    ("MagnifyingGlassParticipant" . "General/MagnifyingGlass")
    ("ChimeSDK" . "BusinessApplications/ChimeSDK")
    ("ChimeSDKParticipant" . "BusinessApplications/ChimeSDK")
    ("Analytics" . "Analytics/Analytics")
    ("AnalyticsParticipant" . "Analytics/Analytics")
    ("CloudFormationStack" . "ManagementGovernance/CloudFormationStack")
    ("CloudFormationStackParticipant"
     . "ManagementGovernance/CloudFormationStack")
    ("ElasticContainerRegistryImage"
     . "Containers/ElasticContainerRegistryImage")
    ("ElasticContainerRegistryImageParticipant"
     . "Containers/ElasticContainerRegistryImage")
    ("NimbleStudio" . "MediaServices/NimbleStudio")
    ("NimbleStudioParticipant" . "MediaServices/NimbleStudio")
    ("ControlTower" . "ManagementGovernance/ControlTower")
    ("ControlTowerParticipant" . "ManagementGovernance/ControlTower")
    ("CloudFormation" . "ManagementGovernance/CloudFormation")
    ("CloudFormationParticipant" . "ManagementGovernance/CloudFormation")
    ("AutoScaling" . "ManagementGovernance/AutoScaling")
    ("AutoScalingParticipant" . "ManagementGovernance/AutoScaling")
    ("ComprehendMedical" . "MachineLearning/ComprehendMedical")
    ("ComprehendMedicalParticipant" . "MachineLearning/ComprehendMedical")
    ("FraudDetector" . "MachineLearning/FraudDetector")
    ("FraudDetectorParticipant" . "MachineLearning/FraudDetector")
    ("SimpleStorageServiceS3IntelligentTiering"
     . "Storage/SimpleStorageServiceS3IntelligentTiering")
    ("SimpleStorageServiceS3IntelligentTieringParticipant"
     . "Storage/SimpleStorageServiceS3IntelligentTiering")
    ("GlueElasticViews" . "Analytics/GlueElasticViews")
    ("GlueElasticViewsParticipant" . "Analytics/GlueElasticViews")
    ("WorkDocsSDK" . "BusinessApplications/WorkDocsSDK")
    ("WorkDocsSDKParticipant" . "BusinessApplications/WorkDocsSDK")
    ("CloudWatchEventEventBased"
     . "ManagementGovernance/CloudWatchEventEventBased")
    ("CloudWatchEventEventBasedParticipant"
     . "ManagementGovernance/CloudWatchEventEventBased")
    ("ElasticKubernetesService" . "Containers/ElasticKubernetesService")
    ("ElasticKubernetesServiceParticipant"
     . "Containers/ElasticKubernetesService")
    ("ElementalLink" . "MediaServices/ElementalLink")
    ("ElementalLinkParticipant" . "MediaServices/ElementalLink")
    ("LicenseManagerApplicationDiscovery"
     . "ManagementGovernance/LicenseManagerApplicationDiscovery")
    ("LicenseManagerApplicationDiscoveryParticipant"
     . "ManagementGovernance/LicenseManagerApplicationDiscovery")
    ("SystemsManagerChangeManager"
     . "ManagementGovernance/SystemsManagerChangeManager")
    ("SystemsManagerChangeManagerParticipant"
     . "ManagementGovernance/SystemsManagerChangeManager")
    ("AppConfig" . "ManagementGovernance/AppConfig")
    ("AppConfigParticipant" . "ManagementGovernance/AppConfig")
    ("SageMaker" . "MachineLearning/SageMaker")
    ("SageMakerParticipant" . "MachineLearning/SageMaker")
    ("MachineLearning" . "MachineLearning/MachineLearning")
    ("MachineLearningParticipant" . "MachineLearning/MachineLearning")
    ("ElementalMediaPackage" . "MediaServices/ElementalMediaPackage")
    ("ElementalMediaPackageParticipant"
     . "MediaServices/ElementalMediaPackage")
    ("SourceCode" . "General/SourceCode")
    ("SourceCodeParticipant" . "General/SourceCode")
    ("PinpointJourney" . "BusinessApplications/PinpointJourney")
    ("PinpointJourneyParticipant"
     . "BusinessApplications/PinpointJourney")
    ("EMRCluster" . "Analytics/EMRCluster")
    ("EMRClusterParticipant" . "Analytics/EMRCluster")
    ("OpsWorksStack2" . "ManagementGovernance/OpsWorksStack2")
    ("OpsWorksStack2Participant" . "ManagementGovernance/OpsWorksStack2")
    ("ElasticContainerServiceContainer3"
     . "Containers/ElasticContainerServiceContainer3")
    ("ElasticContainerServiceContainer3Participant"
     . "Containers/ElasticContainerServiceContainer3")
    ("Proton" . "ManagementGovernance/Proton")
    ("ProtonParticipant" . "ManagementGovernance/Proton")
    ("OpsWorksResources" . "ManagementGovernance/OpsWorksResources")
    ("OpsWorksResourcesParticipant"
     . "ManagementGovernance/OpsWorksResources")
    ("ManagementGovernance" . "ManagementGovernance/ManagementGovernance")
    ("ManagementGovernanceParticipant"
     . "ManagementGovernance/ManagementGovernance")
    ("DevOpsGuruInsights" . "MachineLearning/DevOpsGuruInsights")
    ("DevOpsGuruInsightsParticipant"
     . "MachineLearning/DevOpsGuruInsights")
    ("TensorFlowonAWS" . "MachineLearning/TensorFlowonAWS")
    ("TensorFlowonAWSParticipant" . "MachineLearning/TensorFlowonAWS")
    ("ElementalLive" . "MediaServices/ElementalLive")
    ("ElementalLiveParticipant" . "MediaServices/ElementalLive")
    ("Officebuilding" . "General/Officebuilding")
    ("OfficebuildingParticipant" . "General/Officebuilding")
    ("Honeycode" . "BusinessApplications/Honeycode")
    ("HoneycodeParticipant" . "BusinessApplications/Honeycode")
    ("AppFlow" . "ApplicationIntegration/AppFlow")
    ("AppFlowParticipant" . "ApplicationIntegration/AppFlow")
    ("SystemsManagerCompliance"
     . "ManagementGovernance/SystemsManagerCompliance")
    ("SystemsManagerComplianceParticipant"
     . "ManagementGovernance/SystemsManagerCompliance")
    ("ElasticContainerServiceContainer2"
     . "Containers/ElasticContainerServiceContainer2")
    ("ElasticContainerServiceContainer2Participant"
     . "Containers/ElasticContainerServiceContainer2")
    ("OrganizationsAccount" . "ManagementGovernance/OrganizationsAccount")
    ("OrganizationsAccountParticipant"
     . "ManagementGovernance/OrganizationsAccount")
    ("ElementalMediaConnect" . "MediaServices/ElementalMediaConnect")
    ("ElementalMediaConnectParticipant"
     . "MediaServices/ElementalMediaConnect")
    ("SystemsManagerChangeCalendar"
     . "ManagementGovernance/SystemsManagerChangeCalendar")
    ("SystemsManagerChangeCalendarParticipant"
     . "ManagementGovernance/SystemsManagerChangeCalendar")
    ("TrustedAdvisorChecklistSecurity"
     . "ManagementGovernance/TrustedAdvisorChecklistSecurity")
    ("TrustedAdvisorChecklistSecurityParticipant"
     . "ManagementGovernance/TrustedAdvisorChecklistSecurity")
    ("DeepComposer" . "MachineLearning/DeepComposer")
    ("DeepComposerParticipant" . "MachineLearning/DeepComposer")
    ("LookoutforEquipment" . "MachineLearning/LookoutforEquipment")
    ("LookoutforEquipmentParticipant"
     . "MachineLearning/LookoutforEquipment")
    ("ElementalAppliancesSoftware"
     . "MediaServices/ElementalAppliancesSoftware")
    ("ElementalAppliancesSoftwareParticipant"
     . "MediaServices/ElementalAppliancesSoftware")
    ("Shield2" . "General/Shield2")
    ("Shield2Participant" . "General/Shield2")
    ("WorkMail" . "BusinessApplications/WorkMail")
    ("WorkMailParticipant" . "BusinessApplications/WorkMail")
    ("SimpleNotificationServiceEmailNotification"
     . "ApplicationIntegration/SimpleNotificationServiceEmailNotification")
    ("SimpleNotificationServiceEmailNotificationParticipant"
     . "ApplicationIntegration/SimpleNotificationServiceEmailNotification")
    ("OpsWorksApps" . "ManagementGovernance/OpsWorksApps")
    ("OpsWorksAppsParticipant" . "ManagementGovernance/OpsWorksApps")
    ("ElasticContainerServiceService"
     . "Containers/ElasticContainerServiceService")
    ("ElasticContainerServiceServiceParticipant"
     . "Containers/ElasticContainerServiceService")
    ("ElementalMediaConvert" . "MediaServices/ElementalMediaConvert")
    ("ElementalMediaConvertParticipant"
     . "MediaServices/ElementalMediaConvert")
    ("SystemsManagerAutomation"
     . "ManagementGovernance/SystemsManagerAutomation")
    ("SystemsManagerAutomationParticipant"
     . "ManagementGovernance/SystemsManagerAutomation")
    ("Organizations" . "ManagementGovernance/Organizations")
    ("OrganizationsParticipant" . "ManagementGovernance/Organizations")
    ("CloudWatchEvidently" . "ManagementGovernance/CloudWatchEvidently")
    ("CloudWatchEvidentlyParticipant"
     . "ManagementGovernance/CloudWatchEvidently")
    ("CodeWhisperer" . "MachineLearning/CodeWhisperer")
    ("CodeWhispererParticipant" . "MachineLearning/CodeWhisperer")
    ("RekognitionImage" . "MachineLearning/RekognitionImage")
    ("RekognitionImageParticipant" . "MachineLearning/RekognitionImage")
    ("InteractiveVideoService" . "MediaServices/InteractiveVideoService")
    ("InteractiveVideoServiceParticipant"
     . "MediaServices/InteractiveVideoService")
    ("Connect" . "BusinessApplications/Connect")
    ("ConnectParticipant" . "BusinessApplications/Connect")
    ("User" . "General/User") ("UserParticipant" . "General/User")
    ("APIGatewayEndpoint" . "ApplicationIntegration/APIGatewayEndpoint")
    ("APIGatewayEndpointParticipant"
     . "ApplicationIntegration/APIGatewayEndpoint")
    ("CloudWatchAlarm" . "ManagementGovernance/CloudWatchAlarm")
    ("CloudWatchAlarmParticipant"
     . "ManagementGovernance/CloudWatchAlarm")
    ("ElementalMediaTailor" . "MediaServices/ElementalMediaTailor")
    ("ElementalMediaTailorParticipant"
     . "MediaServices/ElementalMediaTailor")
    ("SystemsManagerPatchManager"
     . "ManagementGovernance/SystemsManagerPatchManager")
    ("SystemsManagerPatchManagerParticipant"
     . "ManagementGovernance/SystemsManagerPatchManager")
    ("ManagementConsole" . "ManagementGovernance/ManagementConsole")
    ("ManagementConsoleParticipant"
     . "ManagementGovernance/ManagementConsole")
    ("OpsWorks" . "ManagementGovernance/OpsWorks")
    ("OpsWorksParticipant" . "ManagementGovernance/OpsWorks")
    ("SageMakerCanvas" . "MachineLearning/SageMakerCanvas")
    ("SageMakerCanvasParticipant" . "MachineLearning/SageMakerCanvas")
    ("DeepRacer" . "MachineLearning/DeepRacer")
    ("DeepRacerParticipant" . "MachineLearning/DeepRacer")
    ("GroundStation" . "Satellite/GroundStation")
    ("GroundStationParticipant" . "Satellite/GroundStation")
    ("Document" . "General/Document")
    ("DocumentParticipant" . "General/Document")
    ("EventBridgeDefaultEventBus"
     . "ApplicationIntegration/EventBridgeDefaultEventBus")
    ("EventBridgeDefaultEventBusParticipant"
     . "ApplicationIntegration/EventBridgeDefaultEventBus")
    ("SimpleNotificationServiceTopic"
     . "ApplicationIntegration/SimpleNotificationServiceTopic")
    ("SimpleNotificationServiceTopicParticipant"
     . "ApplicationIntegration/SimpleNotificationServiceTopic")
    ("WellArchitectedTool" . "ManagementGovernance/WellArchitectedTool")
    ("WellArchitectedToolParticipant"
     . "ManagementGovernance/WellArchitectedTool")
    ("EKSAnywhere" . "Containers/EKSAnywhere")
    ("EKSAnywhereParticipant" . "Containers/EKSAnywhere")
    ("OrganizationsOrganizationalUnit"
     . "ManagementGovernance/OrganizationsOrganizationalUnit")
    ("OrganizationsOrganizationalUnitParticipant"
     . "ManagementGovernance/OrganizationsOrganizationalUnit")
    ("CloudDigitalInterface" . "MediaServices/CloudDigitalInterface")
    ("CloudDigitalInterfaceParticipant"
     . "MediaServices/CloudDigitalInterface")
    ("BackintAgent" . "ManagementGovernance/BackintAgent")
    ("BackintAgentParticipant" . "ManagementGovernance/BackintAgent")
    ("ManagedGrafana" . "ManagementGovernance/ManagedGrafana")
    ("ManagedGrafanaParticipant" . "ManagementGovernance/ManagedGrafana")
    ("Panorama" . "MachineLearning/Panorama")
    ("PanoramaParticipant" . "MachineLearning/Panorama")
    ("Polly" . "MachineLearning/Polly")
    ("PollyParticipant" . "MachineLearning/Polly")
    ("Multimedia" . "General/Multimedia")
    ("MultimediaParticipant" . "General/Multimedia")
    ("EventBridgeSchema" . "ApplicationIntegration/EventBridgeSchema")
    ("EventBridgeSchemaParticipant"
     . "ApplicationIntegration/EventBridgeSchema")
    ("ManagedWorkflowsforApacheAirflow"
     . "ApplicationIntegration/ManagedWorkflowsforApacheAirflow")
    ("ManagedWorkflowsforApacheAirflowParticipant"
     . "ApplicationIntegration/ManagedWorkflowsforApacheAirflow")
    ("LicenseManager" . "ManagementGovernance/LicenseManager")
    ("LicenseManagerParticipant" . "ManagementGovernance/LicenseManager")
    ("RedHatOpenShift" . "Containers/RedHatOpenShift")
    ("RedHatOpenShiftParticipant" . "Containers/RedHatOpenShift")
    ("ManagedServiceforPrometheus"
     . "ManagementGovernance/ManagedServiceforPrometheus")
    ("ManagedServiceforPrometheusParticipant"
     . "ManagementGovernance/ManagedServiceforPrometheus")
    ("ElementalServer" . "MediaServices/ElementalServer")
    ("ElementalServerParticipant" . "MediaServices/ElementalServer")
    ("SystemsManagerStateManager"
     . "ManagementGovernance/SystemsManagerStateManager")
    ("SystemsManagerStateManagerParticipant"
     . "ManagementGovernance/SystemsManagerStateManager")
    ("LaunchWizard" . "ManagementGovernance/LaunchWizard")
    ("LaunchWizardParticipant" . "ManagementGovernance/LaunchWizard")
    ("Personalize" . "MachineLearning/Personalize")
    ("PersonalizeParticipant" . "MachineLearning/Personalize")
    ("Documents" . "General/Documents")
    ("DocumentsParticipant" . "General/Documents")
    ("Translate" . "MachineLearning/Translate")
    ("TranslateParticipant" . "MachineLearning/Translate")
    ("Satellite" . "Satellite/Satellite")
    ("SatelliteParticipant" . "Satellite/Satellite")
    ("ApplicationIntegration"
     . "ApplicationIntegration/ApplicationIntegration")
    ("ApplicationIntegrationParticipant"
     . "ApplicationIntegration/ApplicationIntegration")
    ("EventBridgeEvent" . "ApplicationIntegration/EventBridgeEvent")
    ("EventBridgeEventParticipant"
     . "ApplicationIntegration/EventBridgeEvent")
    ("LicenseManagerLicenseBlending"
     . "ManagementGovernance/LicenseManagerLicenseBlending")
    ("LicenseManagerLicenseBlendingParticipant"
     . "ManagementGovernance/LicenseManagerLicenseBlending")
    ("ElasticContainerServiceCopilotCLI"
     . "Containers/ElasticContainerServiceCopilotCLI")
    ("ElasticContainerServiceCopilotCLIParticipant"
     . "Containers/ElasticContainerServiceCopilotCLI")
    ("OpsWorksMonitoring" . "ManagementGovernance/OpsWorksMonitoring")
    ("OpsWorksMonitoringParticipant"
     . "ManagementGovernance/OpsWorksMonitoring")
    ("ElementalMediaLive" . "MediaServices/ElementalMediaLive")
    ("ElementalMediaLiveParticipant" . "MediaServices/ElementalMediaLive")
    ("CloudFormationTemplate"
     . "ManagementGovernance/CloudFormationTemplate")
    ("CloudFormationTemplateParticipant"
     . "ManagementGovernance/CloudFormationTemplate")
    ("PersonalHealthDashboard"
     . "ManagementGovernance/PersonalHealthDashboard")
    ("PersonalHealthDashboardParticipant"
     . "ManagementGovernance/PersonalHealthDashboard")
    ("SageMakerNotebook" . "MachineLearning/SageMakerNotebook")
    ("SageMakerNotebookParticipant" . "MachineLearning/SageMakerNotebook")
    ("MarketplaceDark" . "General/MarketplaceDark")
    ("MarketplaceDarkParticipant" . "General/MarketplaceDark")
    ("TorchServe" . "MachineLearning/TorchServe")
    ("TorchServeParticipant" . "MachineLearning/TorchServe")
    ("IdentityAccessManagementIAMAccessAnalyzer"
     . "SecurityIdentityCompliance/IdentityAccessManagementIAMAccessAnalyzer")
    ("IdentityAccessManagementIAMAccessAnalyzerParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementIAMAccessAnalyzer")
    ("EventBridgeSaasPartnerEvent"
     . "ApplicationIntegration/EventBridgeSaasPartnerEvent")
    ("EventBridgeSaasPartnerEventParticipant"
     . "ApplicationIntegration/EventBridgeSaasPartnerEvent")
    ("EventBridgeCustomEventBus"
     . "ApplicationIntegration/EventBridgeCustomEventBus")
    ("EventBridgeCustomEventBusParticipant"
     . "ApplicationIntegration/EventBridgeCustomEventBus")
    ("DistroforOpenTelemetry"
     . "ManagementGovernance/DistroforOpenTelemetry")
    ("DistroforOpenTelemetryParticipant"
     . "ManagementGovernance/DistroforOpenTelemetry")
    ("Fargate" . "Containers/Fargate")
    ("FargateParticipant" . "Containers/Fargate")
    ("SystemsManagerInventory"
     . "ManagementGovernance/SystemsManagerInventory")
    ("SystemsManagerInventoryParticipant"
     . "ManagementGovernance/SystemsManagerInventory")
    ("ElementalConductor" . "MediaServices/ElementalConductor")
    ("ElementalConductorParticipant" . "MediaServices/ElementalConductor")
    ("SystemsManagerOpsCenter"
     . "ManagementGovernance/SystemsManagerOpsCenter")
    ("SystemsManagerOpsCenterParticipant"
     . "ManagementGovernance/SystemsManagerOpsCenter")
    ("Config" . "ManagementGovernance/Config")
    ("ConfigParticipant" . "ManagementGovernance/Config")
    ("ElasticInference" . "MachineLearning/ElasticInference")
    ("ElasticInferenceParticipant" . "MachineLearning/ElasticInference")
    ("SSLpadlock" . "General/SSLpadlock")
    ("SSLpadlockParticipant" . "General/SSLpadlock")
    ("DirectoryServiceADConnector"
     . "SecurityIdentityCompliance/DirectoryServiceADConnector")
    ("DirectoryServiceADConnectorParticipant"
     . "SecurityIdentityCompliance/DirectoryServiceADConnector")
    ("ResourceAccessManager"
     . "SecurityIdentityCompliance/ResourceAccessManager")
    ("ResourceAccessManagerParticipant"
     . "SecurityIdentityCompliance/ResourceAccessManager")
    ("SimpleNotificationService"
     . "ApplicationIntegration/SimpleNotificationService")
    ("SimpleNotificationServiceParticipant"
     . "ApplicationIntegration/SimpleNotificationService")
    ("MQ" . "ApplicationIntegration/MQ")
    ("MQParticipant" . "ApplicationIntegration/MQ")
    ("Containers" . "Containers/Containers")
    ("ContainersParticipant" . "Containers/Containers")
    ("OpsWorksDeployments" . "ManagementGovernance/OpsWorksDeployments")
    ("OpsWorksDeploymentsParticipant"
     . "ManagementGovernance/OpsWorksDeployments")
    ("ElementalDelta" . "MediaServices/ElementalDelta")
    ("ElementalDeltaParticipant" . "MediaServices/ElementalDelta")
    ("OpsWorksLayers" . "ManagementGovernance/OpsWorksLayers")
    ("OpsWorksLayersParticipant" . "ManagementGovernance/OpsWorksLayers")
    ("TrustedAdvisorChecklistCost"
     . "ManagementGovernance/TrustedAdvisorChecklistCost")
    ("TrustedAdvisorChecklistCostParticipant"
     . "ManagementGovernance/TrustedAdvisorChecklistCost")
    ("Tapestorage" . "General/Tapestorage")
    ("TapestorageParticipant" . "General/Tapestorage")
    ("CertificateManagerCertificateAuthority"
     . "SecurityIdentityCompliance/CertificateManagerCertificateAuthority")
    ("CertificateManagerCertificateAuthorityParticipant"
     . "SecurityIdentityCompliance/CertificateManagerCertificateAuthority")
    ("WAFManagedRule" . "SecurityIdentityCompliance/WAFManagedRule")
    ("WAFManagedRuleParticipant"
     . "SecurityIdentityCompliance/WAFManagedRule")
    ("KeyManagementService"
     . "SecurityIdentityCompliance/KeyManagementService")
    ("KeyManagementServiceParticipant"
     . "SecurityIdentityCompliance/KeyManagementService")
    ("SimpleNotificationServiceHTTPNotification"
     . "ApplicationIntegration/SimpleNotificationServiceHTTPNotification")
    ("SimpleNotificationServiceHTTPNotificationParticipant"
     . "ApplicationIntegration/SimpleNotificationServiceHTTPNotification")
    ("EventBridgeSchemaRegistry"
     . "ApplicationIntegration/EventBridgeSchemaRegistry")
    ("EventBridgeSchemaRegistryParticipant"
     . "ApplicationIntegration/EventBridgeSchemaRegistry")
    ("EKSDistro" . "Containers/EKSDistro")
    ("EKSDistroParticipant" . "Containers/EKSDistro")
    ("SystemsManagerParameterStore"
     . "ManagementGovernance/SystemsManagerParameterStore")
    ("SystemsManagerParameterStoreParticipant"
     . "ManagementGovernance/SystemsManagerParameterStore")
    ("TrustedAdvisorChecklistFaultTolerant"
     . "ManagementGovernance/TrustedAdvisorChecklistFaultTolerant")
    ("TrustedAdvisorChecklistFaultTolerantParticipant"
     . "ManagementGovernance/TrustedAdvisorChecklistFaultTolerant")
    ("ElementalMediaStore" . "MediaServices/ElementalMediaStore")
    ("ElementalMediaStoreParticipant"
     . "MediaServices/ElementalMediaStore")
    ("ResilienceHub" . "ManagementGovernance/ResilienceHub")
    ("ResilienceHubParticipant" . "ManagementGovernance/ResilienceHub")
    ("FaultInjectionSimulator"
     . "ManagementGovernance/FaultInjectionSimulator")
    ("FaultInjectionSimulatorParticipant"
     . "ManagementGovernance/FaultInjectionSimulator")
    ("AWSManagementConsole" . "General/AWSManagementConsole")
    ("AWSManagementConsoleParticipant" . "General/AWSManagementConsole")
    ("IdentityandAccessManagement"
     . "SecurityIdentityCompliance/IdentityandAccessManagement")
    ("IdentityandAccessManagementParticipant"
     . "SecurityIdentityCompliance/IdentityandAccessManagement")
    ("DirectoryService" . "SecurityIdentityCompliance/DirectoryService")
    ("DirectoryServiceParticipant"
     . "SecurityIdentityCompliance/DirectoryService")
    ("NetworkFirewallEndpoints"
     . "SecurityIdentityCompliance/NetworkFirewallEndpoints")
    ("NetworkFirewallEndpointsParticipant"
     . "SecurityIdentityCompliance/NetworkFirewallEndpoints")
    ("APIGateway" . "ApplicationIntegration/APIGateway")
    ("APIGatewayParticipant" . "ApplicationIntegration/APIGateway")
    ("EKSCloud" . "Containers/EKSCloud")
    ("EKSCloudParticipant" . "Containers/EKSCloud")
    ("CloudTrail" . "ManagementGovernance/CloudTrail")
    ("CloudTrailParticipant" . "ManagementGovernance/CloudTrail")
    ("OpsWorksPermissions" . "ManagementGovernance/OpsWorksPermissions")
    ("OpsWorksPermissionsParticipant"
     . "ManagementGovernance/OpsWorksPermissions")
    ("ElasticContainerRegistryRegistry"
     . "Containers/ElasticContainerRegistryRegistry")
    ("ElasticContainerRegistryRegistryParticipant"
     . "Containers/ElasticContainerRegistryRegistry")
    ("SystemsManagerSessionManager"
     . "ManagementGovernance/SystemsManagerSessionManager")
    ("SystemsManagerSessionManagerParticipant"
     . "ManagementGovernance/SystemsManagerSessionManager")
    ("CloudWatchSynthetics" . "ManagementGovernance/CloudWatchSynthetics")
    ("CloudWatchSyntheticsParticipant"
     . "ManagementGovernance/CloudWatchSynthetics")
    ("Traditionalserver" . "General/Traditionalserver")
    ("TraditionalserverParticipant" . "General/Traditionalserver")
    ("CloudDirectory" . "SecurityIdentityCompliance/CloudDirectory")
    ("CloudDirectoryParticipant"
     . "SecurityIdentityCompliance/CloudDirectory")
    ("IdentityAccessManagementAWSSTS"
     . "SecurityIdentityCompliance/IdentityAccessManagementAWSSTS")
    ("IdentityAccessManagementAWSSTSParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementAWSSTS")
    ("SecurityIdentityCompliance"
     . "SecurityIdentityCompliance/SecurityIdentityCompliance")
    ("SecurityIdentityComplianceParticipant"
     . "SecurityIdentityCompliance/SecurityIdentityCompliance")
    ("SimpleQueueServiceMessage"
     . "ApplicationIntegration/SimpleQueueServiceMessage")
    ("SimpleQueueServiceMessageParticipant"
     . "ApplicationIntegration/SimpleQueueServiceMessage")
    ("ElasticContainerServiceTask"
     . "Containers/ElasticContainerServiceTask")
    ("ElasticContainerServiceTaskParticipant"
     . "Containers/ElasticContainerServiceTask")
    ("EventBridge" . "ApplicationIntegration/EventBridge")
    ("EventBridgeParticipant" . "ApplicationIntegration/EventBridge")
    ("TrustedAdvisorChecklistPerformance"
     . "ManagementGovernance/TrustedAdvisorChecklistPerformance")
    ("TrustedAdvisorChecklistPerformanceParticipant"
     . "ManagementGovernance/TrustedAdvisorChecklistPerformance")
    ("SystemsManagerRunCommand"
     . "ManagementGovernance/SystemsManagerRunCommand")
    ("SystemsManagerRunCommandParticipant"
     . "ManagementGovernance/SystemsManagerRunCommand")
    ("ECSAnywhere" . "Containers/ECSAnywhere")
    ("ECSAnywhereParticipant" . "Containers/ECSAnywhere")
    ("AppSync" . "ApplicationIntegration/AppSync")
    ("AppSyncParticipant" . "ApplicationIntegration/AppSync")
    ("AWS_COLOR #232F3E" . "AWSRaw") ("AWS_BG_COLOR #FFFFFF" . "AWSRaw")
    ("AWS_BORDER_COLOR #FF9900" . "AWSRaw")
    ("AWS_SYMBOL_COLOR AWS_COLOR" . "AWSRaw")
    ("TECHN_FONT_SIZE 12" . "AWSRaw") ("Globe" . "General/Globe")
    ("GlobeParticipant" . "General/Globe")
    ("SecurityHub" . "SecurityIdentityCompliance/SecurityHub")
    ("SecurityHubParticipant" . "SecurityIdentityCompliance/SecurityHub")
    ("Artifact" . "SecurityIdentityCompliance/Artifact")
    ("ArtifactParticipant" . "SecurityIdentityCompliance/Artifact")
    ("WAF" . "SecurityIdentityCompliance/WAF")
    ("WAFParticipant" . "SecurityIdentityCompliance/WAF")
    ("SimpleQueueService" . "ApplicationIntegration/SimpleQueueService")
    ("SimpleQueueServiceParticipant"
     . "ApplicationIntegration/SimpleQueueService")
    ("ElasticContainerService" . "Containers/ElasticContainerService")
    ("ElasticContainerServiceParticipant"
     . "Containers/ElasticContainerService")
    ("EventBridgeRule" . "ApplicationIntegration/EventBridgeRule")
    ("EventBridgeRuleParticipant"
     . "ApplicationIntegration/EventBridgeRule")
    ("MQBroker" . "ApplicationIntegration/MQBroker")
    ("MQBrokerParticipant" . "ApplicationIntegration/MQBroker")
    ("CodeArtifact" . "DeveloperTools/CodeArtifact")
    ("CodeArtifactParticipant" . "DeveloperTools/CodeArtifact")
    ("ConsoleMobileApplication"
     . "ApplicationIntegration/ConsoleMobileApplication")
    ("ConsoleMobileApplicationParticipant"
     . "ApplicationIntegration/ConsoleMobileApplication")
    ("Cloud9Cloud9" . "DeveloperTools/Cloud9Cloud9")
    ("Cloud9Cloud9Participant" . "DeveloperTools/Cloud9Cloud9")
    ("MarketplaceLight" . "General/MarketplaceLight")
    ("MarketplaceLightParticipant" . "General/MarketplaceLight")
    ("DirectoryServiceAWSManagedMicrosoftAD"
     . "SecurityIdentityCompliance/DirectoryServiceAWSManagedMicrosoftAD")
    ("DirectoryServiceAWSManagedMicrosoftADParticipant"
     . "SecurityIdentityCompliance/DirectoryServiceAWSManagedMicrosoftAD")
    ("IdentityAccessManagementEncryptedData"
     . "SecurityIdentityCompliance/IdentityAccessManagementEncryptedData")
    ("IdentityAccessManagementEncryptedDataParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementEncryptedData")
    ("IdentityAccessManagementAWSSTSAlternate"
     . "SecurityIdentityCompliance/IdentityAccessManagementAWSSTSAlternate")
    ("IdentityAccessManagementAWSSTSAlternateParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementAWSSTSAlternate")
    ("StepFunctions" . "ApplicationIntegration/StepFunctions")
    ("StepFunctionsParticipant" . "ApplicationIntegration/StepFunctions")
    ("ElasticContainerServiceContainer1"
     . "Containers/ElasticContainerServiceContainer1")
    ("ElasticContainerServiceContainer1Participant"
     . "Containers/ElasticContainerServiceContainer1")
    ("SDK" . "General/SDK") ("SDKParticipant" . "General/SDK")
    ("ExpressWorkflows" . "ApplicationIntegration/ExpressWorkflows")
    ("ExpressWorkflowsParticipant"
     . "ApplicationIntegration/ExpressWorkflows")
    ("CodePipeline" . "DeveloperTools/CodePipeline")
    ("CodePipelineParticipant" . "DeveloperTools/CodePipeline")
    ("SimpleQueueServiceQueue"
     . "ApplicationIntegration/SimpleQueueServiceQueue")
    ("SimpleQueueServiceQueueParticipant"
     . "ApplicationIntegration/SimpleQueueServiceQueue")
    ("WorkSpacesWeb" . "EndUserComputing/WorkSpacesWeb")
    ("WorkSpacesWebParticipant" . "EndUserComputing/WorkSpacesWeb")
    ("CodeBuild" . "DeveloperTools/CodeBuild")
    ("CodeBuildParticipant" . "DeveloperTools/CodeBuild")
    ("Users" . "General/Users") ("UsersParticipant" . "General/Users")
    ("NetworkFirewall" . "SecurityIdentityCompliance/NetworkFirewall")
    ("NetworkFirewallParticipant"
     . "SecurityIdentityCompliance/NetworkFirewall")
    ("Question" . "General/Question")
    ("QuestionParticipant" . "General/Question")
    ("SecurityHubFinding"
     . "SecurityIdentityCompliance/SecurityHubFinding")
    ("SecurityHubFindingParticipant"
     . "SecurityIdentityCompliance/SecurityHubFinding")
    ("CloudShell" . "DeveloperTools/CloudShell")
    ("CloudShellParticipant" . "DeveloperTools/CloudShell")
    ("Chat" . "General/Chat") ("ChatParticipant" . "General/Chat")
    ("Gear" . "General/Gear") ("GearParticipant" . "General/Gear")
    ("CodeCommit" . "DeveloperTools/CodeCommit")
    ("CodeCommitParticipant" . "DeveloperTools/CodeCommit")
    ("Genericdatabase" . "General/Genericdatabase")
    ("GenericdatabaseParticipant" . "General/Genericdatabase")
    ("WorkLink" . "EndUserComputing/WorkLink")
    ("WorkLinkParticipant" . "EndUserComputing/WorkLink")
    ("CommandLineInterface" . "DeveloperTools/CommandLineInterface")
    ("CommandLineInterfaceParticipant"
     . "DeveloperTools/CommandLineInterface")
    ("Alert" . "General/Alert") ("AlertParticipant" . "General/Alert")
    ("Inspector" . "SecurityIdentityCompliance/Inspector")
    ("InspectorParticipant" . "SecurityIdentityCompliance/Inspector")
    ("WAFBot" . "SecurityIdentityCompliance/WAFBot")
    ("WAFBotParticipant" . "SecurityIdentityCompliance/WAFBot")
    ("Folder" . "General/Folder")
    ("FolderParticipant" . "General/Folder")
    ("DirectoryServiceSimpleAD"
     . "SecurityIdentityCompliance/DirectoryServiceSimpleAD")
    ("DirectoryServiceSimpleADParticipant"
     . "SecurityIdentityCompliance/DirectoryServiceSimpleAD")
    ("Toolkit" . "General/Toolkit")
    ("ToolkitParticipant" . "General/Toolkit")
    ("CodeDeploy" . "DeveloperTools/CodeDeploy")
    ("CodeDeployParticipant" . "DeveloperTools/CodeDeploy")
    ("Mobileclient" . "General/Mobileclient")
    ("MobileclientParticipant" . "General/Mobileclient")
    ("CloudControlAPI" . "DeveloperTools/CloudControlAPI")
    ("CloudControlAPIParticipant" . "DeveloperTools/CloudControlAPI")
    ("Firewall" . "General/Firewall")
    ("FirewallParticipant" . "General/Firewall")
    ("Cloud9" . "DeveloperTools/Cloud9")
    ("Cloud9Participant" . "DeveloperTools/Cloud9")
    ("WorkSpaces" . "EndUserComputing/WorkSpaces")
    ("WorkSpacesParticipant" . "EndUserComputing/WorkSpaces")
    ("Folders" . "General/Folders")
    ("FoldersParticipant" . "General/Folders")
    ("GuardDuty" . "SecurityIdentityCompliance/GuardDuty")
    ("GuardDutyParticipant" . "SecurityIdentityCompliance/GuardDuty")
    ("Detective" . "SecurityIdentityCompliance/Detective")
    ("DetectiveParticipant" . "SecurityIdentityCompliance/Detective")
    ("Forums" . "General/Forums")
    ("ForumsParticipant" . "General/Forums")
    ("ShieldAWSShieldAdvanced"
     . "SecurityIdentityCompliance/ShieldAWSShieldAdvanced")
    ("ShieldAWSShieldAdvancedParticipant"
     . "SecurityIdentityCompliance/ShieldAWSShieldAdvanced")
    ("Internetalt1" . "General/Internetalt1")
    ("Internetalt1Participant" . "General/Internetalt1")
    ("Servers" . "General/Servers")
    ("ServersParticipant" . "General/Servers")
    ("DeveloperTools" . "DeveloperTools/DeveloperTools")
    ("DeveloperToolsParticipant" . "DeveloperTools/DeveloperTools")
    ("ToolsandSDKs" . "DeveloperTools/ToolsandSDKs")
    ("ToolsandSDKsParticipant" . "DeveloperTools/ToolsandSDKs")
    ("EndUserComputing" . "EndUserComputing/EndUserComputing")
    ("EndUserComputingParticipant" . "EndUserComputing/EndUserComputing")
    ("EC2C5adInstance" . "Compute/EC2C5adInstance")
    ("EC2C5adInstanceParticipant" . "Compute/EC2C5adInstance")
    ("Macie" . "SecurityIdentityCompliance/Macie")
    ("MacieParticipant" . "SecurityIdentityCompliance/Macie")
    ("IAMIdentityCenter" . "SecurityIdentityCompliance/IAMIdentityCenter")
    ("IAMIdentityCenterParticipant"
     . "SecurityIdentityCompliance/IAMIdentityCenter")
    ("EC2X2gdInstance" . "Compute/EC2X2gdInstance")
    ("EC2X2gdInstanceParticipant" . "Compute/EC2X2gdInstance")
    ("SecretsManager" . "SecurityIdentityCompliance/SecretsManager")
    ("SecretsManagerParticipant"
     . "SecurityIdentityCompliance/SecretsManager")
    ("Camera" . "General/Camera")
    ("CameraParticipant" . "General/Camera")
    ("EC2M5dnInstance" . "Compute/EC2M5dnInstance")
    ("EC2M5dnInstanceParticipant" . "Compute/EC2M5dnInstance")
    ("CodeStar" . "DeveloperTools/CodeStar")
    ("CodeStarParticipant" . "DeveloperTools/CodeStar")
    ("Corretto" . "DeveloperTools/Corretto")
    ("CorrettoParticipant" . "DeveloperTools/Corretto")
    ("XRay" . "DeveloperTools/XRay")
    ("XRayParticipant" . "DeveloperTools/XRay")
    ("AppStream" . "EndUserComputing/AppStream")
    ("AppStreamParticipant" . "EndUserComputing/AppStream")
    ("Sumerian" . "VRAR/Sumerian")
    ("SumerianParticipant" . "VRAR/Sumerian")
    ("EC2Im4gnInstance" . "Compute/EC2Im4gnInstance")
    ("EC2Im4gnInstanceParticipant" . "Compute/EC2Im4gnInstance")
    ("WAFLabels" . "SecurityIdentityCompliance/WAFLabels")
    ("WAFLabelsParticipant" . "SecurityIdentityCompliance/WAFLabels")
    ("Cognito" . "SecurityIdentityCompliance/Cognito")
    ("CognitoParticipant" . "SecurityIdentityCompliance/Cognito")
    ("EC2R5aInstance" . "Compute/EC2R5aInstance")
    ("EC2R5aInstanceParticipant" . "Compute/EC2R5aInstance")
    ("EC2M4Instance" . "Compute/EC2M4Instance")
    ("EC2M4InstanceParticipant" . "Compute/EC2M4Instance")
    ("IoTTopic" . "InternetOfThings/IoTTopic")
    ("IoTTopicParticipant" . "InternetOfThings/IoTTopic")
    ("EC2C6gnInstance" . "Compute/EC2C6gnInstance")
    ("EC2C6gnInstanceParticipant" . "Compute/EC2C6gnInstance")
    ("CloudDevelopmentKit" . "DeveloperTools/CloudDevelopmentKit")
    ("CloudDevelopmentKitParticipant"
     . "DeveloperTools/CloudDevelopmentKit")
    ("BraketSimulator2" . "QuantumTechnologies/BraketSimulator2")
    ("BraketSimulator2Participant"
     . "QuantumTechnologies/BraketSimulator2")
    ("BraketSimulator4" . "QuantumTechnologies/BraketSimulator4")
    ("BraketSimulator4Participant"
     . "QuantumTechnologies/BraketSimulator4")
    ("BraketChip" . "QuantumTechnologies/BraketChip")
    ("BraketChipParticipant" . "QuantumTechnologies/BraketChip")
    ("VRAR" . "VRAR/VRAR") ("VRARParticipant" . "VRAR/VRAR")
    ("InspectorAgent" . "SecurityIdentityCompliance/InspectorAgent")
    ("InspectorAgentParticipant"
     . "SecurityIdentityCompliance/InspectorAgent")
    ("LambdaLambdaFunction" . "Compute/LambdaLambdaFunction")
    ("LambdaLambdaFunctionParticipant" . "Compute/LambdaLambdaFunction")
    ("EC2I3enInstance" . "Compute/EC2I3enInstance")
    ("EC2I3enInstanceParticipant" . "Compute/EC2I3enInstance")
    ("IdentityAccessManagementTemporarySecurityCredential"
     . "SecurityIdentityCompliance/IdentityAccessManagementTemporarySecurityCredential")
    ("IdentityAccessManagementTemporarySecurityCredentialParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementTemporarySecurityCredential")
    ("EC2H1Instance" . "Compute/EC2H1Instance")
    ("EC2H1InstanceParticipant" . "Compute/EC2H1Instance")
    ("EC2I3Instance" . "Compute/EC2I3Instance")
    ("EC2I3InstanceParticipant" . "Compute/EC2I3Instance")
    ("IoTReportedState" . "InternetOfThings/IoTReportedState")
    ("IoTReportedStateParticipant" . "InternetOfThings/IoTReportedState")
    ("BraketStateVector" . "QuantumTechnologies/BraketStateVector")
    ("BraketStateVectorParticipant"
     . "QuantumTechnologies/BraketStateVector")
    ("BraketNoiseSimulator" . "QuantumTechnologies/BraketNoiseSimulator")
    ("BraketNoiseSimulatorParticipant"
     . "QuantumTechnologies/BraketNoiseSimulator")
    ("QuantumTechnologies" . "QuantumTechnologies/QuantumTechnologies")
    ("QuantumTechnologiesParticipant"
     . "QuantumTechnologies/QuantumTechnologies")
    ("Braket" . "QuantumTechnologies/Braket")
    ("BraketParticipant" . "QuantumTechnologies/Braket")
    ("Wavelength" . "Compute/Wavelength")
    ("WavelengthParticipant" . "Compute/Wavelength")
    ("IdentityAccessManagementAddon"
     . "SecurityIdentityCompliance/IdentityAccessManagementAddon")
    ("IdentityAccessManagementAddonParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementAddon")
    ("EC2M5aInstance" . "Compute/EC2M5aInstance")
    ("EC2M5aInstanceParticipant" . "Compute/EC2M5aInstance")
    ("EC2G5Instance" . "Compute/EC2G5Instance")
    ("EC2G5InstanceParticipant" . "Compute/EC2G5Instance")
    ("IdentityAccessManagementRole"
     . "SecurityIdentityCompliance/IdentityAccessManagementRole")
    ("IdentityAccessManagementRoleParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementRole")
    ("EC2M5dInstance" . "Compute/EC2M5dInstance")
    ("EC2M5dInstanceParticipant" . "Compute/EC2M5dInstance")
    ("Bottlerocket" . "Compute/Bottlerocket")
    ("BottlerocketParticipant" . "Compute/Bottlerocket")
    ("IoTThingMedicalEmergency"
     . "InternetOfThings/IoTThingMedicalEmergency")
    ("IoTThingMedicalEmergencyParticipant"
     . "InternetOfThings/IoTThingMedicalEmergency")
    ("BraketSimulator3" . "QuantumTechnologies/BraketSimulator3")
    ("BraketSimulator3Participant"
     . "QuantumTechnologies/BraketSimulator3")
    ("BraketManagedSimulator"
     . "QuantumTechnologies/BraketManagedSimulator")
    ("BraketManagedSimulatorParticipant"
     . "QuantumTechnologies/BraketManagedSimulator")
    ("" . "AWSC4Integration")
    ("BraketSimulator" . "QuantumTechnologies/BraketSimulator")
    ("BraketSimulatorParticipant" . "QuantumTechnologies/BraketSimulator")
    ("FirewallManager" . "SecurityIdentityCompliance/FirewallManager")
    ("FirewallManagerParticipant"
     . "SecurityIdentityCompliance/FirewallManager")
    ("EC2X1eInstance" . "Compute/EC2X1eInstance")
    ("EC2X1eInstanceParticipant" . "Compute/EC2X1eInstance")
    ("EC2DL1Instance" . "Compute/EC2DL1Instance")
    ("EC2DL1InstanceParticipant" . "Compute/EC2DL1Instance")
    ("EC2ElasticIPAddress" . "Compute/EC2ElasticIPAddress")
    ("EC2ElasticIPAddressParticipant" . "Compute/EC2ElasticIPAddress")
    ("IdentityAccessManagementLongTermSecurityCredential"
     . "SecurityIdentityCompliance/IdentityAccessManagementLongTermSecurityCredential")
    ("IdentityAccessManagementLongTermSecurityCredentialParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementLongTermSecurityCredential")
    ("EC2DBInstance" . "Compute/EC2DBInstance")
    ("EC2DBInstanceParticipant" . "Compute/EC2DBInstance")
    ("IoTThingPLC" . "InternetOfThings/IoTThingPLC")
    ("IoTThingPLCParticipant" . "InternetOfThings/IoTThingPLC")
    ("BraketQPU" . "QuantumTechnologies/BraketQPU")
    ("BraketQPUParticipant" . "QuantumTechnologies/BraketQPU")
    ("BraketChandelier" . "QuantumTechnologies/BraketChandelier")
    ("BraketChandelierParticipant"
     . "QuantumTechnologies/BraketChandelier")
    ("BraketEmbeddedSimulator"
     . "QuantumTechnologies/BraketEmbeddedSimulator")
    ("BraketEmbeddedSimulatorParticipant"
     . "QuantumTechnologies/BraketEmbeddedSimulator")
    ("Region" . "GroupIcons/Region")
    ("RegionParticipant" . "GroupIcons/Region")
    ("BraketSimulator1" . "QuantumTechnologies/BraketSimulator1")
    ("BraketSimulator1Participant"
     . "QuantumTechnologies/BraketSimulator1")
    ("WAFRule" . "SecurityIdentityCompliance/WAFRule")
    ("WAFRuleParticipant" . "SecurityIdentityCompliance/WAFRule")
    ("EC2C5nInstance" . "Compute/EC2C5nInstance")
    ("EC2C5nInstanceParticipant" . "Compute/EC2C5nInstance")
    ("ElasticBeanstalk" . "Compute/ElasticBeanstalk")
    ("ElasticBeanstalkParticipant" . "Compute/ElasticBeanstalk")
    ("EC2P3dnInstance" . "Compute/EC2P3dnInstance")
    ("EC2P3dnInstanceParticipant" . "Compute/EC2P3dnInstance")
    ("AuditManager" . "SecurityIdentityCompliance/AuditManager")
    ("AuditManagerParticipant"
     . "SecurityIdentityCompliance/AuditManager")
    ("EC2AWSMicroserviceExtractorforNET"
     . "Compute/EC2AWSMicroserviceExtractorforNET")
    ("EC2AWSMicroserviceExtractorforNETParticipant"
     . "Compute/EC2AWSMicroserviceExtractorforNET")
    ("IoTAlexaVoiceService" . "InternetOfThings/IoTAlexaVoiceService")
    ("IoTAlexaVoiceServiceParticipant"
     . "InternetOfThings/IoTAlexaVoiceService")
    ("BraketTensorNetwork" . "QuantumTechnologies/BraketTensorNetwork")
    ("BraketTensorNetworkParticipant"
     . "QuantumTechnologies/BraketTensorNetwork")
    ("Cloud" . "GroupIcons/Cloud")
    ("CloudParticipant" . "GroupIcons/Cloud")
    ("ElasticBeanstalkContainer" . "GroupIcons/ElasticBeanstalkContainer")
    ("ElasticBeanstalkContainerParticipant"
     . "GroupIcons/ElasticBeanstalkContainer")
    ("SpotFleet" . "GroupIcons/SpotFleet")
    ("SpotFleetParticipant" . "GroupIcons/SpotFleet")
    ("CorporateDataCenter" . "GroupIcons/CorporateDataCenter")
    ("CorporateDataCenterParticipant" . "GroupIcons/CorporateDataCenter")
    ("IdentityAccessManagementDataEncryptionKey"
     . "SecurityIdentityCompliance/IdentityAccessManagementDataEncryptionKey")
    ("IdentityAccessManagementDataEncryptionKeyParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementDataEncryptionKey")
    ("EC2M5Instance" . "Compute/EC2M5Instance")
    ("EC2M5InstanceParticipant" . "Compute/EC2M5Instance")
    ("EC2HMIInstance" . "Compute/EC2HMIInstance")
    ("EC2HMIInstanceParticipant" . "Compute/EC2HMIInstance")
    ("ThinkboxFrost" . "Compute/ThinkboxFrost")
    ("ThinkboxFrostParticipant" . "Compute/ThinkboxFrost")
    ("WAFFilteringRule" . "SecurityIdentityCompliance/WAFFilteringRule")
    ("WAFFilteringRuleParticipant"
     . "SecurityIdentityCompliance/WAFFilteringRule")
    ("EC2M6gdInstance" . "Compute/EC2M6gdInstance")
    ("EC2M6gdInstanceParticipant" . "Compute/EC2M6gdInstance")
    ("IoTGreengrass" . "InternetOfThings/IoTGreengrass")
    ("IoTGreengrassParticipant" . "InternetOfThings/IoTGreengrass")
    ("VPCSubnetPrivate" . "GroupIcons/VPCSubnetPrivate")
    ("VPCSubnetPrivateParticipant" . "GroupIcons/VPCSubnetPrivate")
    ("EC2InstanceContainer" . "GroupIcons/EC2InstanceContainer")
    ("EC2InstanceContainerParticipant"
     . "GroupIcons/EC2InstanceContainer")
    ("StepFunction" . "GroupIcons/StepFunction")
    ("StepFunctionParticipant" . "GroupIcons/StepFunction")
    ("VirtualPrivateCloudVPC" . "GroupIcons/VirtualPrivateCloudVPC")
    ("VirtualPrivateCloudVPCParticipant"
     . "GroupIcons/VirtualPrivateCloudVPC")
    ("CloudHSM" . "SecurityIdentityCompliance/CloudHSM")
    ("CloudHSMParticipant" . "SecurityIdentityCompliance/CloudHSM")
    ("EC2X2ieznInstance" . "Compute/EC2X2ieznInstance")
    ("EC2X2ieznInstanceParticipant" . "Compute/EC2X2ieznInstance")
    ("EC2HabanaGaudiInstance" . "Compute/EC2HabanaGaudiInstance")
    ("EC2HabanaGaudiInstanceParticipant"
     . "Compute/EC2HabanaGaudiInstance")
    ("GenomicsCLI" . "Compute/GenomicsCLI")
    ("GenomicsCLIParticipant" . "Compute/GenomicsCLI")
    ("WAFBadBot" . "SecurityIdentityCompliance/WAFBadBot")
    ("WAFBadBotParticipant" . "SecurityIdentityCompliance/WAFBadBot")
    ("EC2ImageBuilder" . "Compute/EC2ImageBuilder")
    ("EC2ImageBuilderParticipant" . "Compute/EC2ImageBuilder")
    ("IoTDesiredState" . "InternetOfThings/IoTDesiredState")
    ("IoTDesiredStateParticipant" . "InternetOfThings/IoTDesiredState")
    ("EC2Trn1Instance" . "Compute/EC2Trn1Instance")
    ("EC2Trn1InstanceParticipant" . "Compute/EC2Trn1Instance")
    ("ServerContents" . "GroupIcons/ServerContents")
    ("ServerContentsParticipant" . "GroupIcons/ServerContents")
    ("Cloudalt" . "GroupIcons/Cloudalt")
    ("CloudaltParticipant" . "GroupIcons/Cloudalt")
    ("EC2C7gInstance" . "Compute/EC2C7gInstance")
    ("EC2C7gInstanceParticipant" . "Compute/EC2C7gInstance")
    ("AutoScalingGroup" . "GroupIcons/AutoScalingGroup")
    ("AutoScalingGroupParticipant" . "GroupIcons/AutoScalingGroup")
    ("Shield" . "SecurityIdentityCompliance/Shield")
    ("ShieldParticipant" . "SecurityIdentityCompliance/Shield")
    ("EC2C5dInstance" . "Compute/EC2C5dInstance")
    ("EC2C5dInstanceParticipant" . "Compute/EC2C5dInstance")
    ("Lightsail" . "Compute/Lightsail")
    ("LightsailParticipant" . "Compute/Lightsail")
    ("EC2I2Instance" . "Compute/EC2I2Instance")
    ("EC2I2InstanceParticipant" . "Compute/EC2I2Instance")
    ("WAFBotControl" . "SecurityIdentityCompliance/WAFBotControl")
    ("WAFBotControlParticipant"
     . "SecurityIdentityCompliance/WAFBotControl")
    ("ComputeOptimizer" . "Compute/ComputeOptimizer")
    ("ComputeOptimizerParticipant" . "Compute/ComputeOptimizer")
    ("IoTRule" . "InternetOfThings/IoTRule")
    ("IoTRuleParticipant" . "InternetOfThings/IoTRule")
    ("EC2P4dInstance" . "Compute/EC2P4dInstance")
    ("EC2P4dInstanceParticipant" . "Compute/EC2P4dInstance")
    ("VPCSubnetPublic" . "GroupIcons/VPCSubnetPublic")
    ("VPCSubnetPublicParticipant" . "GroupIcons/VPCSubnetPublic")
    ("EC2P2Instance" . "Compute/EC2P2Instance")
    ("EC2P2InstanceParticipant" . "Compute/EC2P2Instance")
    ("ServerlessApplicationRepository"
     . "Compute/ServerlessApplicationRepository")
    ("ServerlessApplicationRepositoryParticipant"
     . "Compute/ServerlessApplicationRepository")
    ("EC2A1Instance" . "Compute/EC2A1Instance")
    ("EC2A1InstanceParticipant" . "Compute/EC2A1Instance")
    ("EC2R5nInstance" . "Compute/EC2R5nInstance")
    ("EC2R5nInstanceParticipant" . "Compute/EC2R5nInstance")
    ("CertificateManager"
     . "SecurityIdentityCompliance/CertificateManager")
    ("CertificateManagerParticipant"
     . "SecurityIdentityCompliance/CertificateManager")
    ("Compute" . "Compute/Compute")
    ("ComputeParticipant" . "Compute/Compute")
    ("EC2InstancewithCloudWatch" . "Compute/EC2InstancewithCloudWatch")
    ("EC2InstancewithCloudWatchParticipant"
     . "Compute/EC2InstancewithCloudWatch")
    ("IdentityAccessManagementPermissions"
     . "SecurityIdentityCompliance/IdentityAccessManagementPermissions")
    ("IdentityAccessManagementPermissionsParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementPermissions")
    ("EC2R5gdInstance" . "Compute/EC2R5gdInstance")
    ("EC2R5gdInstanceParticipant" . "Compute/EC2R5gdInstance")
    ("IoTLambdaFunction" . "InternetOfThings/IoTLambdaFunction")
    ("IoTLambdaFunctionParticipant"
     . "InternetOfThings/IoTLambdaFunction")
    ("EC2X2idnInstance" . "Compute/EC2X2idnInstance")
    ("EC2X2idnInstanceParticipant" . "Compute/EC2X2idnInstance")
    ("EC2M1MacInstance" . "Compute/EC2M1MacInstance")
    ("EC2M1MacInstanceParticipant" . "Compute/EC2M1MacInstance")
    ("ThinkboxXMesh" . "Compute/ThinkboxXMesh")
    ("ThinkboxXMeshParticipant" . "Compute/ThinkboxXMesh")
    ("EC2X2iednInstance" . "Compute/EC2X2iednInstance")
    ("EC2X2iednInstanceParticipant" . "Compute/EC2X2iednInstance")
    ("Outpostsservers" . "Compute/Outpostsservers")
    ("OutpostsserversParticipant" . "Compute/Outpostsservers")
    ("ParallelCluster" . "Compute/ParallelCluster")
    ("ParallelClusterParticipant" . "Compute/ParallelCluster")
    ("IdentityAccessManagementMFAToken"
     . "SecurityIdentityCompliance/IdentityAccessManagementMFAToken")
    ("IdentityAccessManagementMFATokenParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementMFAToken")
    ("EC2C6aInstance" . "Compute/EC2C6aInstance")
    ("EC2C6aInstanceParticipant" . "Compute/EC2C6aInstance")
    ("EC2Instances" . "Compute/EC2Instances")
    ("EC2InstancesParticipant" . "Compute/EC2Instances")
    ("EC2T3aInstance" . "Compute/EC2T3aInstance")
    ("EC2T3aInstanceParticipant" . "Compute/EC2T3aInstance")
    ("Signer" . "SecurityIdentityCompliance/Signer")
    ("SignerParticipant" . "SecurityIdentityCompliance/Signer")
    ("IoTThingPoliceEmergency"
     . "InternetOfThings/IoTThingPoliceEmergency")
    ("IoTThingPoliceEmergencyParticipant"
     . "InternetOfThings/IoTThingPoliceEmergency")
    ("EC2T3Instance" . "Compute/EC2T3Instance")
    ("EC2T3InstanceParticipant" . "Compute/EC2T3Instance")
    ("EC2D2Instance" . "Compute/EC2D2Instance")
    ("EC2D2InstanceParticipant" . "Compute/EC2D2Instance")
    ("EC2F1Instance" . "Compute/EC2F1Instance")
    ("EC2F1InstanceParticipant" . "Compute/EC2F1Instance")
    ("Batch" . "Compute/Batch") ("BatchParticipant" . "Compute/Batch")
    ("EC2M6aInstance" . "Compute/EC2M6aInstance")
    ("EC2M6aInstanceParticipant" . "Compute/EC2M6aInstance")
    ("EC2P4deInstance" . "Compute/EC2P4deInstance")
    ("EC2P4deInstanceParticipant" . "Compute/EC2P4deInstance")
    ("IdentityAccessManagementIAMRolesAnywhere"
     . "SecurityIdentityCompliance/IdentityAccessManagementIAMRolesAnywhere")
    ("IdentityAccessManagementIAMRolesAnywhereParticipant"
     . "SecurityIdentityCompliance/IdentityAccessManagementIAMRolesAnywhere")
    ("EC2X1Instance" . "Compute/EC2X1Instance")
    ("EC2X1InstanceParticipant" . "Compute/EC2X1Instance")
    ("EC2VT1Instance" . "Compute/EC2VT1Instance")
    ("EC2VT1InstanceParticipant" . "Compute/EC2VT1Instance")
    ("EC2" . "Compute/EC2") ("EC2Participant" . "Compute/EC2")
    ("EC2G4adInstance" . "Compute/EC2G4adInstance")
    ("EC2G4adInstanceParticipant" . "Compute/EC2G4adInstance")
    ("VMwareCloudonAWS" . "Compute/VMwareCloudonAWS")
    ("VMwareCloudonAWSParticipant" . "Compute/VMwareCloudonAWS")
    ("IoTGreengrassInterprocessCommunication"
     . "InternetOfThings/IoTGreengrassInterprocessCommunication")
    ("IoTGreengrassInterprocessCommunicationParticipant"
     . "InternetOfThings/IoTGreengrassInterprocessCommunication")
    ("EC2R4Instance" . "Compute/EC2R4Instance")
    ("EC2R4InstanceParticipant" . "Compute/EC2R4Instance")
    ("EC2R5dInstance" . "Compute/EC2R5dInstance")
    ("EC2R5dInstanceParticipant" . "Compute/EC2R5dInstance")
    ("EC2C4Instance" . "Compute/EC2C4Instance")
    ("EC2C4InstanceParticipant" . "Compute/EC2C4Instance")
    ("ThinkboxKrakatoa" . "Compute/ThinkboxKrakatoa")
    ("ThinkboxKrakatoaParticipant" . "Compute/ThinkboxKrakatoa")
    ("ThinkboxSequoia" . "Compute/ThinkboxSequoia")
    ("ThinkboxSequoiaParticipant" . "Compute/ThinkboxSequoia")
    ("EC2R6iInstance" . "Compute/EC2R6iInstance")
    ("EC2R6iInstanceParticipant" . "Compute/EC2R6iInstance")
    ("NitroEnclaves" . "Compute/NitroEnclaves")
    ("NitroEnclavesParticipant" . "Compute/NitroEnclaves")
    ("AppRunner" . "Compute/AppRunner")
    ("AppRunnerParticipant" . "Compute/AppRunner")
    ("ElasticBeanstalkApplication"
     . "Compute/ElasticBeanstalkApplication")
    ("ElasticBeanstalkApplicationParticipant"
     . "Compute/ElasticBeanstalkApplication")
    ("ApplicationAutoScaling" . "Compute/ApplicationAutoScaling")
    ("ApplicationAutoScalingParticipant"
     . "Compute/ApplicationAutoScaling")
    ("EC2C6iInstance" . "Compute/EC2C6iInstance")
    ("EC2C6iInstanceParticipant" . "Compute/EC2C6iInstance")
    ("IoTSiteWiseAssetModel" . "InternetOfThings/IoTSiteWiseAssetModel")
    ("IoTSiteWiseAssetModelParticipant"
     . "InternetOfThings/IoTSiteWiseAssetModel")
    ("ThinkboxStoke" . "Compute/ThinkboxStoke")
    ("ThinkboxStokeParticipant" . "Compute/ThinkboxStoke")
    ("EC2R5Instance" . "Compute/EC2R5Instance")
    ("EC2R5InstanceParticipant" . "Compute/EC2R5Instance")
    ("EC2M5nInstance" . "Compute/EC2M5nInstance")
    ("EC2M5nInstanceParticipant" . "Compute/EC2M5nInstance")
    ("EC2G4dnInstance" . "Compute/EC2G4dnInstance")
    ("EC2G4dnInstanceParticipant" . "Compute/EC2G4dnInstance")
    ("NICEDCV" . "Compute/NICEDCV")
    ("NICEDCVParticipant" . "Compute/NICEDCV")
    ("Lambda" . "Compute/Lambda")
    ("LambdaParticipant" . "Compute/Lambda")
    ("Outpostsfamily" . "Compute/Outpostsfamily")
    ("OutpostsfamilyParticipant" . "Compute/Outpostsfamily")
    ("EC2C6gInstance" . "Compute/EC2C6gInstance")
    ("EC2C6gInstanceParticipant" . "Compute/EC2C6gInstance")
    ("EC2Inf1Instance" . "Compute/EC2Inf1Instance")
    ("EC2Inf1InstanceParticipant" . "Compute/EC2Inf1Instance")
    ("EC2D3enInstance" . "Compute/EC2D3enInstance")
    ("EC2D3enInstanceParticipant" . "Compute/EC2D3enInstance")
    ("EC2Is4genInstance" . "Compute/EC2Is4genInstance")
    ("EC2Is4genInstanceParticipant" . "Compute/EC2Is4genInstance")
    ("IoTAnalyticsNotebook" . "InternetOfThings/IoTAnalyticsNotebook")
    ("IoTAnalyticsNotebookParticipant"
     . "InternetOfThings/IoTAnalyticsNotebook")
    ("EC2D3Instance" . "Compute/EC2D3Instance")
    ("EC2D3InstanceParticipant" . "Compute/EC2D3Instance")
    ("EC2Instance" . "Compute/EC2Instance")
    ("EC2InstanceParticipant" . "Compute/EC2Instance")
    ("EC2C6gdInstance" . "Compute/EC2C6gdInstance")
    ("EC2C6gdInstanceParticipant" . "Compute/EC2C6gdInstance")
    ("EC2C5Instance" . "Compute/EC2C5Instance")
    ("EC2C5InstanceParticipant" . "Compute/EC2C5Instance")
    ("EC2AMI" . "Compute/EC2AMI")
    ("EC2AMIParticipant" . "Compute/EC2AMI")
    ("EC2Hpc6aInstance" . "Compute/EC2Hpc6aInstance")
    ("EC2Hpc6aInstanceParticipant" . "Compute/EC2Hpc6aInstance")
    ("Outpostsrack" . "Compute/Outpostsrack")
    ("OutpostsrackParticipant" . "Compute/Outpostsrack")
    ("LocalZones" . "Compute/LocalZones")
    ("LocalZonesParticipant" . "Compute/LocalZones")
    ("EC2R6gInstance" . "Compute/EC2R6gInstance")
    ("EC2R6gInstanceParticipant" . "Compute/EC2R6gInstance")
    ("EC2G3Instance" . "Compute/EC2G3Instance")
    ("EC2G3InstanceParticipant" . "Compute/EC2G3Instance")
    ("EC2AutoScaling" . "Compute/EC2AutoScaling")
    ("EC2AutoScalingParticipant" . "Compute/EC2AutoScaling")
    ("IoTThingFactory" . "InternetOfThings/IoTThingFactory")
    ("IoTThingFactoryParticipant" . "InternetOfThings/IoTThingFactory")
    ("EC2z1dInstance" . "Compute/EC2z1dInstance")
    ("EC2z1dInstanceParticipant" . "Compute/EC2z1dInstance")
    ("EC2TrainiumInstance" . "Compute/EC2TrainiumInstance")
    ("EC2TrainiumInstanceParticipant" . "Compute/EC2TrainiumInstance")
    ("EC2Rescue" . "Compute/EC2Rescue")
    ("EC2RescueParticipant" . "Compute/EC2Rescue")
    ("EC2C5aInstance" . "Compute/EC2C5aInstance")
    ("EC2C5aInstanceParticipant" . "Compute/EC2C5aInstance")
    ("EC2M6iInstance" . "Compute/EC2M6iInstance")
    ("EC2M6iInstanceParticipant" . "Compute/EC2M6iInstance")
    ("EC2M6gInstance" . "Compute/EC2M6gInstance")
    ("EC2M6gInstanceParticipant" . "Compute/EC2M6gInstance")
    ("EC2I4iInstance" . "Compute/EC2I4iInstance")
    ("EC2I4iInstanceParticipant" . "Compute/EC2I4iInstance")
    ("IoTGreengrassProtocol" . "InternetOfThings/IoTGreengrassProtocol")
    ("IoTGreengrassProtocolParticipant"
     . "InternetOfThings/IoTGreengrassProtocol")
    ("NICEEnginFrame" . "Compute/NICEEnginFrame")
    ("NICEEnginFrameParticipant" . "Compute/NICEEnginFrame")
    ("EC2M5znInstance" . "Compute/EC2M5znInstance")
    ("EC2M5znInstanceParticipant" . "Compute/EC2M5znInstance")
    ("ElasticBeanstalkDeployment" . "Compute/ElasticBeanstalkDeployment")
    ("ElasticBeanstalkDeploymentParticipant"
     . "Compute/ElasticBeanstalkDeployment")
    ("FreeRTOS" . "InternetOfThings/FreeRTOS")
    ("FreeRTOSParticipant" . "InternetOfThings/FreeRTOS")
    ("EC2AWSInferentia" . "Compute/EC2AWSInferentia")
    ("EC2AWSInferentiaParticipant" . "Compute/EC2AWSInferentia")
    ("Fargate2" . "Compute/Fargate2")
    ("Fargate2Participant" . "Compute/Fargate2")
    ("EC2R5adInstance" . "Compute/EC2R5adInstance")
    ("EC2R5adInstanceParticipant" . "Compute/EC2R5adInstance")
    ("EC2RdnInstance" . "Compute/EC2RdnInstance")
    ("EC2RdnInstanceParticipant" . "Compute/EC2RdnInstance")
    ("IoTFleetWise" . "InternetOfThings/IoTFleetWise")
    ("IoTFleetWiseParticipant" . "InternetOfThings/IoTFleetWise")
    ("IoTServo" . "InternetOfThings/IoTServo")
    ("IoTServoParticipant" . "InternetOfThings/IoTServo")
    ("IoTGreengrassRecipe" . "InternetOfThings/IoTGreengrassRecipe")
    ("IoTGreengrassRecipeParticipant"
     . "InternetOfThings/IoTGreengrassRecipe")
    ("IoTThingTemperatureHumiditySensor"
     . "InternetOfThings/IoTThingTemperatureHumiditySensor")
    ("IoTThingTemperatureHumiditySensorParticipant"
     . "InternetOfThings/IoTThingTemperatureHumiditySensor")
    ("EC2T2Instance" . "Compute/EC2T2Instance")
    ("EC2T2InstanceParticipant" . "Compute/EC2T2Instance")
    ("EC2SpotInstance" . "Compute/EC2SpotInstance")
    ("EC2SpotInstanceParticipant" . "Compute/EC2SpotInstance")
    ("IoTEcho" . "InternetOfThings/IoTEcho")
    ("IoTEchoParticipant" . "InternetOfThings/IoTEcho")
    ("EC2P4Instance" . "Compute/EC2P4Instance")
    ("EC2P4InstanceParticipant" . "Compute/EC2P4Instance")
    ("EC2P3Instance" . "Compute/EC2P3Instance")
    ("EC2P3InstanceParticipant" . "Compute/EC2P3Instance")
    ("EC2T4gInstance" . "Compute/EC2T4gInstance")
    ("EC2T4gInstanceParticipant" . "Compute/EC2T4gInstance")
    ("ThinkboxDeadline" . "Compute/ThinkboxDeadline")
    ("ThinkboxDeadlineParticipant" . "Compute/ThinkboxDeadline")
    ("EC2R5bInstance" . "Compute/EC2R5bInstance")
    ("EC2R5bInstanceParticipant" . "Compute/EC2R5bInstance")
    ("IoTThingsGraph" . "InternetOfThings/IoTThingsGraph")
    ("IoTThingsGraphParticipant" . "InternetOfThings/IoTThingsGraph")
    ("IoTAlexaSkill" . "InternetOfThings/IoTAlexaSkill")
    ("IoTAlexaSkillParticipant" . "InternetOfThings/IoTAlexaSkill")
    ("IoTEduKit" . "InternetOfThings/IoTEduKit")
    ("IoTEduKitParticipant" . "InternetOfThings/IoTEduKit")
    ("ElasticFabricAdapter" . "Compute/ElasticFabricAdapter")
    ("ElasticFabricAdapterParticipant" . "Compute/ElasticFabricAdapter")
    ("IoTThingVibrationSensor"
     . "InternetOfThings/IoTThingVibrationSensor")
    ("IoTThingVibrationSensorParticipant"
     . "InternetOfThings/IoTThingVibrationSensor")
    ("EC2MacInstance" . "Compute/EC2MacInstance")
    ("EC2MacInstanceParticipant" . "Compute/EC2MacInstance")
    ("IoTHTTPProtocol" . "InternetOfThings/IoTHTTPProtocol")
    ("IoTHTTPProtocolParticipant" . "InternetOfThings/IoTHTTPProtocol")
    ("EC2G5gInstance" . "Compute/EC2G5gInstance")
    ("EC2G5gInstanceParticipant" . "Compute/EC2G5gInstance")
    ("IoTThingCart" . "InternetOfThings/IoTThingCart")
    ("IoTThingCartParticipant" . "InternetOfThings/IoTThingCart")
    ("IoTAnalyticsDataStore" . "InternetOfThings/IoTAnalyticsDataStore")
    ("IoTAnalyticsDataStoreParticipant"
     . "InternetOfThings/IoTAnalyticsDataStore")
    ("InternetOfThings" . "InternetOfThings/InternetOfThings")
    ("InternetOfThingsParticipant" . "InternetOfThings/InternetOfThings")
    ("EC2AutoScalingResource" . "Compute/EC2AutoScalingResource")
    ("EC2AutoScalingResourceParticipant"
     . "Compute/EC2AutoScalingResource")
    ("IoTFireTVStick" . "InternetOfThings/IoTFireTVStick")
    ("IoTFireTVStickParticipant" . "InternetOfThings/IoTFireTVStick")
    ("IoTHTTP2Protocol" . "InternetOfThings/IoTHTTP2Protocol")
    ("IoTHTTP2ProtocolParticipant" . "InternetOfThings/IoTHTTP2Protocol")
    ("IoTExpressLink" . "InternetOfThings/IoTExpressLink")
    ("IoTExpressLinkParticipant" . "InternetOfThings/IoTExpressLink")
    ("IoTThingIndustrialPC" . "InternetOfThings/IoTThingIndustrialPC")
    ("IoTThingIndustrialPCParticipant"
     . "InternetOfThings/IoTThingIndustrialPC")
    ("IoTThingBank" . "InternetOfThings/IoTThingBank")
    ("IoTThingBankParticipant" . "InternetOfThings/IoTThingBank")
    ("IoTThingFreeRTOSDevice" . "InternetOfThings/IoTThingFreeRTOSDevice")
    ("IoTThingFreeRTOSDeviceParticipant"
     . "InternetOfThings/IoTThingFreeRTOSDevice")
    ("IoTHardwareBoard" . "InternetOfThings/IoTHardwareBoard")
    ("IoTHardwareBoardParticipant" . "InternetOfThings/IoTHardwareBoard")
    ("IoTAnalytics" . "InternetOfThings/IoTAnalytics")
    ("IoTAnalyticsParticipant" . "InternetOfThings/IoTAnalytics")
    ("IoTThingCar" . "InternetOfThings/IoTThingCar")
    ("IoTThingCarParticipant" . "InternetOfThings/IoTThingCar")
    ("IoTThingThermostat" . "InternetOfThings/IoTThingThermostat")
    ("IoTThingThermostatParticipant"
     . "InternetOfThings/IoTThingThermostat")
    ("IoTThingTemperatureSensor"
     . "InternetOfThings/IoTThingTemperatureSensor")
    ("IoTThingTemperatureSensorParticipant"
     . "InternetOfThings/IoTThingTemperatureSensor")
    ("IoT1Click" . "InternetOfThings/IoT1Click")
    ("IoT1ClickParticipant" . "InternetOfThings/IoT1Click")
    ("IoTGreengrassArtifact" . "InternetOfThings/IoTGreengrassArtifact")
    ("IoTGreengrassArtifactParticipant"
     . "InternetOfThings/IoTGreengrassArtifact")
    ("IoTShadow" . "InternetOfThings/IoTShadow")
    ("IoTShadowParticipant" . "InternetOfThings/IoTShadow")
    ("IoTThingStacklight" . "InternetOfThings/IoTThingStacklight")
    ("IoTThingStacklightParticipant"
     . "InternetOfThings/IoTThingStacklight")
    ("IoTDeviceDefender" . "InternetOfThings/IoTDeviceDefender")
    ("IoTDeviceDefenderParticipant"
     . "InternetOfThings/IoTDeviceDefender")
    ("IoTGreengrassConnector" . "InternetOfThings/IoTGreengrassConnector")
    ("IoTGreengrassConnectorParticipant"
     . "InternetOfThings/IoTGreengrassConnector")
    ("IoTGreengrassComponentNucleus"
     . "InternetOfThings/IoTGreengrassComponentNucleus")
    ("IoTGreengrassComponentNucleusParticipant"
     . "InternetOfThings/IoTGreengrassComponentNucleus")
    ("IoTSensor" . "InternetOfThings/IoTSensor")
    ("IoTSensorParticipant" . "InternetOfThings/IoTSensor")
    ("IoTGreengrassComponentMachineLearning"
     . "InternetOfThings/IoTGreengrassComponentMachineLearning")
    ("IoTGreengrassComponentMachineLearningParticipant"
     . "InternetOfThings/IoTGreengrassComponentMachineLearning")
    ("IoTAnalyticsDataset" . "InternetOfThings/IoTAnalyticsDataset")
    ("IoTAnalyticsDatasetParticipant"
     . "InternetOfThings/IoTAnalyticsDataset")
    ("IoTAnalyticsPipeline" . "InternetOfThings/IoTAnalyticsPipeline")
    ("IoTAnalyticsPipelineParticipant"
     . "InternetOfThings/IoTAnalyticsPipeline")
    ("IoTSiteWise" . "InternetOfThings/IoTSiteWise")
    ("IoTSiteWiseParticipant" . "InternetOfThings/IoTSiteWise")
    ("IoTDeviceDefenderIoTDeviceJobs"
     . "InternetOfThings/IoTDeviceDefenderIoTDeviceJobs")
    ("IoTDeviceDefenderIoTDeviceJobsParticipant"
     . "InternetOfThings/IoTDeviceDefenderIoTDeviceJobs")
    ("IoTFireTV" . "InternetOfThings/IoTFireTV")
    ("IoTFireTVParticipant" . "InternetOfThings/IoTFireTV")
    ("IoTThingCoffeePot" . "InternetOfThings/IoTThingCoffeePot")
    ("IoTThingCoffeePotParticipant"
     . "InternetOfThings/IoTThingCoffeePot")
    ("IoTSailboat" . "InternetOfThings/IoTSailboat")
    ("IoTSailboatParticipant" . "InternetOfThings/IoTSailboat")
    ("IoTPolicy" . "InternetOfThings/IoTPolicy")
    ("IoTPolicyParticipant" . "InternetOfThings/IoTPolicy")
    ("IoTThingDoorLock" . "InternetOfThings/IoTThingDoorLock")
    ("IoTThingDoorLockParticipant" . "InternetOfThings/IoTThingDoorLock")
    ("IoTRoboRunner" . "InternetOfThings/IoTRoboRunner")
    ("IoTRoboRunnerParticipant" . "InternetOfThings/IoTRoboRunner")
    ("IoTActuator" . "InternetOfThings/IoTActuator")
    ("IoTActuatorParticipant" . "InternetOfThings/IoTActuator")
    ("IoTCertificate" . "InternetOfThings/IoTCertificate")
    ("IoTCertificateParticipant" . "InternetOfThings/IoTCertificate")
    ("IoTThingHouse" . "InternetOfThings/IoTThingHouse")
    ("IoTThingHouseParticipant" . "InternetOfThings/IoTThingHouse")
    ("IoTThingTravel" . "InternetOfThings/IoTThingTravel")
    ("IoTThingTravelParticipant" . "InternetOfThings/IoTThingTravel")
    ("IoTThingBicycle" . "InternetOfThings/IoTThingBicycle")
    ("IoTThingBicycleParticipant" . "InternetOfThings/IoTThingBicycle")
    ("IoTAlexaEnabledDevice" . "InternetOfThings/IoTAlexaEnabledDevice")
    ("IoTAlexaEnabledDeviceParticipant"
     . "InternetOfThings/IoTAlexaEnabledDevice")
    ("IoTSimulator" . "InternetOfThings/IoTSimulator")
    ("IoTSimulatorParticipant" . "InternetOfThings/IoTSimulator")
    ("IoTThingRelay" . "InternetOfThings/IoTThingRelay")
    ("IoTThingRelayParticipant" . "InternetOfThings/IoTThingRelay")
    ("IoTMQTTProtocol" . "InternetOfThings/IoTMQTTProtocol")
    ("IoTMQTTProtocolParticipant" . "InternetOfThings/IoTMQTTProtocol")
    ("IoTThingUtility" . "InternetOfThings/IoTThingUtility")
    ("IoTThingUtilityParticipant" . "InternetOfThings/IoTThingUtility")
    ("IoTGreengrassComponent" . "InternetOfThings/IoTGreengrassComponent")
    ("IoTGreengrassComponentParticipant"
     . "InternetOfThings/IoTGreengrassComponent")
    ("IoTTwinMaker" . "InternetOfThings/IoTTwinMaker")
    ("IoTTwinMakerParticipant" . "InternetOfThings/IoTTwinMaker")
    ("IoTDeviceManagementFleetHub"
     . "InternetOfThings/IoTDeviceManagementFleetHub")
    ("IoTDeviceManagementFleetHubParticipant"
     . "InternetOfThings/IoTDeviceManagementFleetHub")
    ("IoTThingLightbulb" . "InternetOfThings/IoTThingLightbulb")
    ("IoTThingLightbulbParticipant"
     . "InternetOfThings/IoTThingLightbulb")
    ("IoTThingHumiditySensor" . "InternetOfThings/IoTThingHumiditySensor")
    ("IoTThingHumiditySensorParticipant"
     . "InternetOfThings/IoTThingHumiditySensor")
    ("IoTThingWindfarm" . "InternetOfThings/IoTThingWindfarm")
    ("IoTThingWindfarmParticipant" . "InternetOfThings/IoTThingWindfarm")
    ("IoTSiteWiseAsset" . "InternetOfThings/IoTSiteWiseAsset")
    ("IoTSiteWiseAssetParticipant" . "InternetOfThings/IoTSiteWiseAsset")
    ("IoTOverAirUpdate" . "InternetOfThings/IoTOverAirUpdate")
    ("IoTOverAirUpdateParticipant" . "InternetOfThings/IoTOverAirUpdate")
    ("IoTThingGeneric" . "InternetOfThings/IoTThingGeneric")
    ("IoTThingGenericParticipant" . "InternetOfThings/IoTThingGeneric")
    ("IoTDeviceGateway" . "InternetOfThings/IoTDeviceGateway")
    ("IoTDeviceGatewayParticipant" . "InternetOfThings/IoTDeviceGateway")
    ("IoTThingCamera" . "InternetOfThings/IoTThingCamera")
    ("IoTThingCameraParticipant" . "InternetOfThings/IoTThingCamera")
    ("IoTSiteWiseAssetProperties"
     . "InternetOfThings/IoTSiteWiseAssetProperties")
    ("IoTSiteWiseAssetPropertiesParticipant"
     . "InternetOfThings/IoTSiteWiseAssetProperties")
    ("IoTAnalyticsChannel" . "InternetOfThings/IoTAnalyticsChannel")
    ("IoTAnalyticsChannelParticipant"
     . "InternetOfThings/IoTAnalyticsChannel")
    ("IoTSiteWiseDataStreams" . "InternetOfThings/IoTSiteWiseDataStreams")
    ("IoTSiteWiseDataStreamsParticipant"
     . "InternetOfThings/IoTSiteWiseDataStreams")
    ("IoTButton" . "InternetOfThings/IoTButton")
    ("IoTButtonParticipant" . "InternetOfThings/IoTButton")
    ("IoTGreengrassComponentPrivate"
     . "InternetOfThings/IoTGreengrassComponentPrivate")
    ("IoTGreengrassComponentPrivateParticipant"
     . "InternetOfThings/IoTGreengrassComponentPrivate")
    ("IoTLoRaWANProtocol" . "InternetOfThings/IoTLoRaWANProtocol")
    ("IoTLoRaWANProtocolParticipant"
     . "InternetOfThings/IoTLoRaWANProtocol")
    ("IoTSiteWiseAssetHierarchy"
     . "InternetOfThings/IoTSiteWiseAssetHierarchy")
    ("IoTSiteWiseAssetHierarchyParticipant"
     . "InternetOfThings/IoTSiteWiseAssetHierarchy")
    ("IoTThingTemperatureVibrationSensor"
     . "InternetOfThings/IoTThingTemperatureVibrationSensor")
    ("IoTThingTemperatureVibrationSensorParticipant"
     . "InternetOfThings/IoTThingTemperatureVibrationSensor")
    ("IoTGreengrassComponentPublic"
     . "InternetOfThings/IoTGreengrassComponentPublic")
    ("IoTGreengrassComponentPublicParticipant"
     . "InternetOfThings/IoTGreengrassComponentPublic")
    ("IoTEvents" . "InternetOfThings/IoTEvents")
    ("IoTEventsParticipant" . "InternetOfThings/IoTEvents")
    ("IoTCore" . "InternetOfThings/IoTCore")
    ("IoTCoreParticipant" . "InternetOfThings/IoTCore")
    ("IoTAction" . "InternetOfThings/IoTAction")
    ("IoTActionParticipant" . "InternetOfThings/IoTAction")
    ("IoTGreengrassStreamManager"
     . "InternetOfThings/IoTGreengrassStreamManager")
    ("IoTGreengrassStreamManagerParticipant"
     . "InternetOfThings/IoTGreengrassStreamManager")
    ("IoTDeviceManagement" . "InternetOfThings/IoTDeviceManagement")
    ("IoTDeviceManagementParticipant"
     . "InternetOfThings/IoTDeviceManagement"))
  "List of PlantUML AWS components")


(cl-defmethod plantuml-stdlib--common-include ((library (eql 'aws)))
  "!include <awslib/AWSCommon>")

(cl-defmethod plantuml-stdlib--prefix ((library (eql 'aws)))
  "awslib")

(cl-defmethod plantuml-stdlib--components ((library (eql 'aws)))
  plantuml-aws-components)

(provide 'plantuml-stdlib-aws)
;;; plantuml-stdlib-aws.el
