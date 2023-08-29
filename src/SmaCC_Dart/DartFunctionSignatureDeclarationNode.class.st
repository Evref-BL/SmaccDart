Class {
	#name : #DartFunctionSignatureDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'signature'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionSignatureDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionSignatureDeclaration: self
]

{ #category : #generated }
DartFunctionSignatureDeclarationNode >> nodeVariables [

	^ #( #signature )
]

{ #category : #generated }
DartFunctionSignatureDeclarationNode >> signature [

	^ signature
]

{ #category : #generated }
DartFunctionSignatureDeclarationNode >> signature: aDartSetterSignatureNode [

	self signature notNil ifTrue: [ self signature parent: nil ].
	signature := aDartSetterSignatureNode.
	self signature notNil ifTrue: [ self signature parent: self ]
]
