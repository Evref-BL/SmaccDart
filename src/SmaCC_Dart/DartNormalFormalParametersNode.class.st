Class {
	#name : #DartNormalFormalParametersNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'parameter',
		'nextParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNormalFormalParametersNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNormalFormalParameters: self
]

{ #category : #generated }
DartNormalFormalParametersNode >> compositeNodeVariables [

	^ #( #nextParameters )
]

{ #category : #'generated-initialize-release' }
DartNormalFormalParametersNode >> initialize [
	super initialize.
	nextParameters := OrderedCollection new: 2.
]

{ #category : #generated }
DartNormalFormalParametersNode >> nextParameters [

	^ nextParameters
]

{ #category : #generated }
DartNormalFormalParametersNode >> nextParameters: anOrderedCollection [

	self setParents: self nextParameters to: nil.
	nextParameters := anOrderedCollection.
	self setParents: self nextParameters to: self
]

{ #category : #generated }
DartNormalFormalParametersNode >> nodeVariables [

	^ #( #parameter )
]

{ #category : #generated }
DartNormalFormalParametersNode >> parameter [

	^ parameter
]

{ #category : #generated }
DartNormalFormalParametersNode >> parameter: aDartNormalFormalParameterNode [

	self parameter notNil ifTrue: [ self parameter parent: nil ].
	parameter := aDartNormalFormalParameterNode.
	self parameter notNil ifTrue: [ self parameter parent: self ]
]
