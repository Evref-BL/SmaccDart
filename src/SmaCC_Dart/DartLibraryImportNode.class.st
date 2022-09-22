Class {
	#name : #DartLibraryImportNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'importSpecification'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLibraryImportNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLibraryImport: self
]

{ #category : #generated }
DartLibraryImportNode >> importSpecification [

	^ importSpecification
]

{ #category : #generated }
DartLibraryImportNode >> importSpecification: aDartImportSpecificationNode [

	self importSpecification notNil ifTrue: [ 
		self importSpecification parent: nil ].
	importSpecification := aDartImportSpecificationNode.
	self importSpecification notNil ifTrue: [ 
		self importSpecification parent: self ]
]

{ #category : #generated }
DartLibraryImportNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartLibraryImportNode >> metadata: anOrderedCollection [

	self setParent: self metadata to: nil.
	metadata := anOrderedCollection.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartLibraryImportNode >> nodeVariables [

	^ #( #importSpecification )
]

{ #category : #generated }
DartLibraryImportNode >> otherVariables [

	^ #( #metadata )
]
