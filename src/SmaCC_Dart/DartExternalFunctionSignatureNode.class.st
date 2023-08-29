Class {
	#name : #DartExternalFunctionSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'signature'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartExternalFunctionSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitExternalFunctionSignature: self
]

{ #category : #generated }
DartExternalFunctionSignatureNode >> nodeVariables [

	^ #( #signature )
]

{ #category : #generated }
DartExternalFunctionSignatureNode >> signature [

	^ signature
]

{ #category : #generated }
DartExternalFunctionSignatureNode >> signature: aDartProgramNode [

	self signature notNil ifTrue: [ self signature parent: nil ].
	signature := aDartProgramNode.
	self signature notNil ifTrue: [ self signature parent: self ]
]
