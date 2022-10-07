Class {
	#name : #DartAssignableExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'primary',
		'assignableSelectorPart',
		'unconditionalAssignableSelector',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAssignableExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAssignableExpression: self
]

{ #category : #generated }
DartAssignableExpressionNode >> assignableSelectorPart [

	^ assignableSelectorPart
]

{ #category : #generated }
DartAssignableExpressionNode >> assignableSelectorPart: anObject [

	self setParent: self assignableSelectorPart to: nil.
	assignableSelectorPart := anObject.
	self setParent: self assignableSelectorPart to: self
]

{ #category : #generated }
DartAssignableExpressionNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartAssignableExpressionNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartAssignableExpressionNode >> nodeVariables [

	^ #( #primary #identifier )
]

{ #category : #generated }
DartAssignableExpressionNode >> otherVariables [

	^ #( #assignableSelectorPart #unconditionalAssignableSelector )
]

{ #category : #generated }
DartAssignableExpressionNode >> primary [

	^ primary
]

{ #category : #generated }
DartAssignableExpressionNode >> primary: aDartProgramNode [

	self primary notNil ifTrue: [ self primary parent: nil ].
	primary := aDartProgramNode.
	self primary notNil ifTrue: [ self primary parent: self ]
]

{ #category : #generated }
DartAssignableExpressionNode >> unconditionalAssignableSelector [

	^ unconditionalAssignableSelector
]

{ #category : #generated }
DartAssignableExpressionNode >> unconditionalAssignableSelector: anObject [

	self setParent: self unconditionalAssignableSelector to: nil.
	unconditionalAssignableSelector := anObject.
	self setParent: self unconditionalAssignableSelector to: self
]
