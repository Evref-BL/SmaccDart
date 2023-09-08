Class {
	#name : #DartNormalFormalParameterNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'parameter'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNormalFormalParameterNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNormalFormalParameter: self
]

{ #category : #generated }
DartNormalFormalParameterNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartNormalFormalParameterNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartNormalFormalParameterNode >> nodeVariables [

	^ #( #parameter )
]

{ #category : #generated }
DartNormalFormalParameterNode >> otherVariables [

	^ #( #metadata )
]

{ #category : #generated }
DartNormalFormalParameterNode >> parameter [

	^ parameter
]

{ #category : #generated }
DartNormalFormalParameterNode >> parameter: aDartProgramNode [

	self parameter notNil ifTrue: [ self parameter parent: nil ].
	parameter := aDartProgramNode.
	self parameter notNil ifTrue: [ self parameter parent: self ]
]
