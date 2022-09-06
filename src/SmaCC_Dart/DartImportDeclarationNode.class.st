Class {
	#name : #DartImportDeclarationNode,
	#superclass : #DartImportExportDeclarationNode,
	#instVars : [
		'metadata',
		'configurableUri',
		'semicolonToken',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartImportDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitImportDeclaration: self
]

{ #category : #generated }
DartImportDeclarationNode >> configurableUri [

	^ configurableUri
]

{ #category : #generated }
DartImportDeclarationNode >> configurableUri: aSmaCCToken [

	configurableUri := aSmaCCToken
]

{ #category : #generated }
DartImportDeclarationNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartImportDeclarationNode >> identifier: anOrderedCollection [

	self setParent: self identifier to: nil.
	identifier := anOrderedCollection.
	self setParent: self identifier to: self
]

{ #category : #generated }
DartImportDeclarationNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartImportDeclarationNode >> metadata: anOrderedCollection [

	self setParent: self metadata to: nil.
	metadata := anOrderedCollection.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartImportDeclarationNode >> otherVariables [

	^ #( #metadata #identifier )
]

{ #category : #generated }
DartImportDeclarationNode >> semicolonToken [

	^ semicolonToken
]

{ #category : #generated }
DartImportDeclarationNode >> semicolonToken: aSmaCCToken [

	semicolonToken := aSmaCCToken
]

{ #category : #generated }
DartImportDeclarationNode >> tokenVariables [

	^ #( #configurableUri #semicolonToken )
]
