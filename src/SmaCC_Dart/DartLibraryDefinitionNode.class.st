Class {
	#name : #DartLibraryDefinitionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'libraryImports',
		'libraryExports',
		'metadatas',
		'uris',
		'topLevelDefinitions',
		'libraryName',
		'scriptTag',
		'feff'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLibraryDefinitionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLibraryDefinition: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> compositeNodeVariables [

	^ #( #libraryImports #libraryExports #uris )
]

{ #category : #generated }
DartLibraryDefinitionNode >> feff [

	^ feff
]

{ #category : #generated }
DartLibraryDefinitionNode >> feff: aSmaCCToken [

	feff := aSmaCCToken
]

{ #category : #'generated-initialize-release' }
DartLibraryDefinitionNode >> initialize [
	super initialize.
	libraryImports := OrderedCollection new: 2.
	libraryExports := OrderedCollection new: 2.
	metadatas := OrderedCollection new: 2.
	uris := OrderedCollection new: 2.
	topLevelDefinitions := OrderedCollection new: 2.
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryExports [

	^ libraryExports
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryExports: anOrderedCollection [

	self setParents: self libraryExports to: nil.
	libraryExports := anOrderedCollection.
	self setParents: self libraryExports to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryImports [

	^ libraryImports
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryImports: anOrderedCollection [

	self setParents: self libraryImports to: nil.
	libraryImports := anOrderedCollection.
	self setParents: self libraryImports to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryName [

	^ libraryName
]

{ #category : #generated }
DartLibraryDefinitionNode >> libraryName: aDartLibraryNameNode [

	self libraryName notNil ifTrue: [ self libraryName parent: nil ].
	libraryName := aDartLibraryNameNode.
	self libraryName notNil ifTrue: [ self libraryName parent: self ]
]

{ #category : #generated }
DartLibraryDefinitionNode >> metadatas [

	^ metadatas
]

{ #category : #generated }
DartLibraryDefinitionNode >> metadatas: anOrderedCollection [

	self setParent: self metadatas to: nil.
	metadatas := anOrderedCollection.
	self setParent: self metadatas to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> nodeVariables [

	^ #( #libraryName )
]

{ #category : #generated }
DartLibraryDefinitionNode >> otherVariables [

	^ #( #metadatas #topLevelDefinitions )
]

{ #category : #generated }
DartLibraryDefinitionNode >> scriptTag [

	^ scriptTag
]

{ #category : #generated }
DartLibraryDefinitionNode >> scriptTag: aSmaCCToken [

	scriptTag := aSmaCCToken
]

{ #category : #generated }
DartLibraryDefinitionNode >> tokenVariables [

	^ #( #scriptTag #feff )
]

{ #category : #generated }
DartLibraryDefinitionNode >> topLevelDefinitions [

	^ topLevelDefinitions
]

{ #category : #generated }
DartLibraryDefinitionNode >> topLevelDefinitions: anOrderedCollection [

	self setParent: self topLevelDefinitions to: nil.
	topLevelDefinitions := anOrderedCollection.
	self setParent: self topLevelDefinitions to: self
]

{ #category : #generated }
DartLibraryDefinitionNode >> uris [

	^ uris
]

{ #category : #generated }
DartLibraryDefinitionNode >> uris: anOrderedCollection [

	self setParents: self uris to: nil.
	uris := anOrderedCollection.
	self setParents: self uris to: self
]
