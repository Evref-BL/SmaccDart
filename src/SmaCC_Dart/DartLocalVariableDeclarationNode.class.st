Class {
	#name : #DartLocalVariableDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'initializedVariableDeclaration'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLocalVariableDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLocalVariableDeclaration: self
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> initializedVariableDeclaration [

	^ initializedVariableDeclaration
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> initializedVariableDeclaration: aDartInitializedVariableDeclarationNode [

	self initializedVariableDeclaration notNil ifTrue: [ 
		self initializedVariableDeclaration parent: nil ].
	initializedVariableDeclaration := aDartInitializedVariableDeclarationNode.
	self initializedVariableDeclaration notNil ifTrue: [ 
		self initializedVariableDeclaration parent: self ]
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> nodeVariables [

	^ #( #initializedVariableDeclaration )
]

{ #category : #generated }
DartLocalVariableDeclarationNode >> otherVariables [

	^ #( #metadata )
]
