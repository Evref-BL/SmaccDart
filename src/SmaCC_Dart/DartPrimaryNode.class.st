Class {
	#name : #DartPrimaryNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'unconditionalAssignableSelector',
		'argumentPart',
		'parenthesisExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartPrimaryNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitPrimary: self
]

{ #category : #generated }
DartPrimaryNode >> argumentPart [

	^ argumentPart
]

{ #category : #generated }
DartPrimaryNode >> argumentPart: anObject [

	self setParent: self argumentPart to: nil.
	argumentPart := anObject.
	self setParent: self argumentPart to: self
]

{ #category : #generated }
DartPrimaryNode >> otherVariables [

	^ #( #unconditionalAssignableSelector
	     #argumentPart #parenthesisExpression )
]

{ #category : #generated }
DartPrimaryNode >> parenthesisExpression [

	^ parenthesisExpression
]

{ #category : #generated }
DartPrimaryNode >> parenthesisExpression: anOrderedCollection [

	self setParent: self parenthesisExpression to: nil.
	parenthesisExpression := anOrderedCollection.
	self setParent: self parenthesisExpression to: self
]

{ #category : #generated }
DartPrimaryNode >> unconditionalAssignableSelector [

	^ unconditionalAssignableSelector
]

{ #category : #generated }
DartPrimaryNode >> unconditionalAssignableSelector: anObject [

	self setParent: self unconditionalAssignableSelector to: nil.
	unconditionalAssignableSelector := anObject.
	self setParent: self unconditionalAssignableSelector to: self
]
