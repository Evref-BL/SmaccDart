Class {
	#name : #DartImportSpecificationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'configurableUri',
		'identifiers',
		'nextIdentifiers'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartImportSpecificationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitImportSpecification: self
]

{ #category : #generated }
DartImportSpecificationNode >> compositeNodeVariables [

	^ #( #identifiers #nextIdentifiers )
]

{ #category : #generated }
DartImportSpecificationNode >> configurableUri [

	^ configurableUri
]

{ #category : #generated }
DartImportSpecificationNode >> configurableUri: aDartConfigurableUriNode [

	self configurableUri notNil ifTrue: [ 
		self configurableUri parent: nil ].
	configurableUri := aDartConfigurableUriNode.
	self configurableUri notNil ifTrue: [ 
		self configurableUri parent: self ]
]

{ #category : #generated }
DartImportSpecificationNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartImportSpecificationNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartImportSpecificationNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
	nextIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartImportSpecificationNode >> nextIdentifiers [

	^ nextIdentifiers
]

{ #category : #generated }
DartImportSpecificationNode >> nextIdentifiers: anOrderedCollection [

	self setParents: self nextIdentifiers to: nil.
	nextIdentifiers := anOrderedCollection.
	self setParents: self nextIdentifiers to: self
]

{ #category : #generated }
DartImportSpecificationNode >> nodeVariables [

	^ #( #configurableUri )
]
