Class {
	#name : #DartOperatorSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'operator',
		'formalParameterList',
		'type'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartOperatorSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitOperatorSignature: self
]

{ #category : #generated }
DartOperatorSignatureNode >> formalParameterList [

	^ formalParameterList
]

{ #category : #generated }
DartOperatorSignatureNode >> formalParameterList: aDartProgramNode [

	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: nil ].
	formalParameterList := aDartProgramNode.
	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: self ]
]

{ #category : #generated }
DartOperatorSignatureNode >> nodeVariables [

	^ #( #formalParameterList #type )
]

{ #category : #generated }
DartOperatorSignatureNode >> operator [

	^ operator
]

{ #category : #generated }
DartOperatorSignatureNode >> operator: aSmaCCToken [

	operator := aSmaCCToken
]

{ #category : #generated }
DartOperatorSignatureNode >> tokenVariables [

	^ #( #operator )
]

{ #category : #generated }
DartOperatorSignatureNode >> type [

	^ type
]

{ #category : #generated }
DartOperatorSignatureNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]
