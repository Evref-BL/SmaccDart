Class {
	#name : #DartLibraryNameNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'dottedIdentifierList'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLibraryNameNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLibraryName: self
]

{ #category : #generated }
DartLibraryNameNode >> dottedIdentifierList [

	^ dottedIdentifierList
]

{ #category : #generated }
DartLibraryNameNode >> dottedIdentifierList: aDartDottedIdentifierListNode [

	self dottedIdentifierList notNil ifTrue: [ 
		self dottedIdentifierList parent: nil ].
	dottedIdentifierList := aDartDottedIdentifierListNode.
	self dottedIdentifierList notNil ifTrue: [ 
		self dottedIdentifierList parent: self ]
]

{ #category : #generated }
DartLibraryNameNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartLibraryNameNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartLibraryNameNode >> nodeVariables [

	^ #( #dottedIdentifierList )
]

{ #category : #generated }
DartLibraryNameNode >> otherVariables [

	^ #( #metadata )
]
