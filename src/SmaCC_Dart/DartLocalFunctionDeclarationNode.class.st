Class {
	#name : #DartLocalFunctionDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'functionSignature',
		'functionBody'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLocalFunctionDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLocalFunctionDeclaration: self
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> functionBody [

	^ functionBody
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> functionBody: aDartProgramNode [

	self functionBody notNil ifTrue: [ self functionBody parent: nil ].
	functionBody := aDartProgramNode.
	self functionBody notNil ifTrue: [ self functionBody parent: self ]
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> functionSignature [

	^ functionSignature
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> functionSignature: aDartFunctionSignatureNode [

	self functionSignature notNil ifTrue: [ 
		self functionSignature parent: nil ].
	functionSignature := aDartFunctionSignatureNode.
	self functionSignature notNil ifTrue: [ 
		self functionSignature parent: self ]
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> nodeVariables [

	^ #( #functionSignature #functionBody )
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> otherVariables [

	^ #( #metadata )
]
