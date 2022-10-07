Class {
	#name : #DartQualifiedNameNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeIdentifiers',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartQualifiedNameNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitQualifiedName: self
]

{ #category : #generated }
DartQualifiedNameNode >> compositeTokenVariables [

	^ #( #typeIdentifiers )
]

{ #category : #generated }
DartQualifiedNameNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartQualifiedNameNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #'generated-initialize-release' }
DartQualifiedNameNode >> initialize [
	super initialize.
	typeIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartQualifiedNameNode >> nodeVariables [

	^ #( #identifier )
]

{ #category : #generated }
DartQualifiedNameNode >> typeIdentifiers [

	^ typeIdentifiers
]

{ #category : #generated }
DartQualifiedNameNode >> typeIdentifiers: anOrderedCollection [

	typeIdentifiers := anOrderedCollection
]
