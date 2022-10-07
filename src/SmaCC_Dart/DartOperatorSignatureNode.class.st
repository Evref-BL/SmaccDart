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
DartOperatorSignatureNode >> formalParameterList: anObject [

	self setParent: self formalParameterList to: nil.
	formalParameterList := anObject.
	self setParent: self formalParameterList to: self
]

{ #category : #generated }
DartOperatorSignatureNode >> nodeVariables [

	^ #( #type )
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
DartOperatorSignatureNode >> otherVariables [

	^ #( #formalParameterList )
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
