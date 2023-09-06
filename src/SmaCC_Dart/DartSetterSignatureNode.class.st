Class {
	#name : #DartSetterSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifier',
		'formalParameterList',
		'type'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSetterSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSetterSignature: self
]

{ #category : #generated }
DartSetterSignatureNode >> formalParameterList [

	^ formalParameterList
]

{ #category : #generated }
DartSetterSignatureNode >> formalParameterList: aDartProgramNode [

	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: nil ].
	formalParameterList := aDartProgramNode.
	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: self ]
]

{ #category : #generated }
DartSetterSignatureNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartSetterSignatureNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartSetterSignatureNode >> nodeVariables [

	^ #( #identifier #formalParameterList #type )
]

{ #category : #generated }
DartSetterSignatureNode >> type [

	^ type
]

{ #category : #generated }
DartSetterSignatureNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]
