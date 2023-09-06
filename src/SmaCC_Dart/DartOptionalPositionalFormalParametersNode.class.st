Class {
	#name : #DartOptionalPositionalFormalParametersNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'parameter',
		'nextParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitOptionalPositionalFormalParameters: self
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> compositeNodeVariables [

	^ #( #nextParameters )
]

{ #category : #'generated-initialize-release' }
DartOptionalPositionalFormalParametersNode >> initialize [
	super initialize.
	nextParameters := OrderedCollection new: 2.
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> nextParameters [

	^ nextParameters
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> nextParameters: anOrderedCollection [

	self setParents: self nextParameters to: nil.
	nextParameters := anOrderedCollection.
	self setParents: self nextParameters to: self
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> nodeVariables [

	^ #( #parameter )
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> parameter [

	^ parameter
]

{ #category : #generated }
DartOptionalPositionalFormalParametersNode >> parameter: aDartNormalFormalParameterNode [

	self parameter notNil ifTrue: [ self parameter parent: nil ].
	parameter := aDartNormalFormalParameterNode.
	self parameter notNil ifTrue: [ self parameter parent: self ]
]
