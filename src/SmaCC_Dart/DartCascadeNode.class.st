Class {
	#name : #DartCascadeNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'cascade',
		'cascadeSection',
		'conditionalExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartCascadeNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitCascade: self
]

{ #category : #generated }
DartCascadeNode >> cascade [

	^ cascade
]

{ #category : #generated }
DartCascadeNode >> cascade: aDartCascadeNode [

	self cascade notNil ifTrue: [ self cascade parent: nil ].
	cascade := aDartCascadeNode.
	self cascade notNil ifTrue: [ self cascade parent: self ]
]

{ #category : #generated }
DartCascadeNode >> cascadeSection [

	^ cascadeSection
]

{ #category : #generated }
DartCascadeNode >> cascadeSection: anObject [

	self setParent: self cascadeSection to: nil.
	cascadeSection := anObject.
	self setParent: self cascadeSection to: self
]

{ #category : #generated }
DartCascadeNode >> conditionalExpression [

	^ conditionalExpression
]

{ #category : #generated }
DartCascadeNode >> conditionalExpression: aDartProgramNode [

	self conditionalExpression notNil ifTrue: [ 
		self conditionalExpression parent: nil ].
	conditionalExpression := aDartProgramNode.
	self conditionalExpression notNil ifTrue: [ 
		self conditionalExpression parent: self ]
]

{ #category : #generated }
DartCascadeNode >> nodeVariables [

	^ #( #cascade #conditionalExpression )
]

{ #category : #generated }
DartCascadeNode >> otherVariables [

	^ #( #cascadeSection )
]
