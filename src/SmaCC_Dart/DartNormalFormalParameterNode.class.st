Class {
	#name : #DartNormalFormalParameterNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'normalFormalParameterNoMetadata'
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

	^ #( #normalFormalParameterNoMetadata )
]

{ #category : #generated }
DartNormalFormalParameterNode >> normalFormalParameterNoMetadata [

	^ normalFormalParameterNoMetadata
]

{ #category : #generated }
DartNormalFormalParameterNode >> normalFormalParameterNoMetadata: aDartProgramNode [

	self normalFormalParameterNoMetadata notNil ifTrue: [ 
		self normalFormalParameterNoMetadata parent: nil ].
	normalFormalParameterNoMetadata := aDartProgramNode.
	self normalFormalParameterNoMetadata notNil ifTrue: [ 
		self normalFormalParameterNoMetadata parent: self ]
]

{ #category : #generated }
DartNormalFormalParameterNode >> otherVariables [

	^ #( #metadata )
]
