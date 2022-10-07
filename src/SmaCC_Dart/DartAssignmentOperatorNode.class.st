Class {
	#name : #DartAssignmentOperatorNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'equalsSymbol',
		'compoundAssignmentOperator'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAssignmentOperatorNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAssignmentOperator: self
]

{ #category : #generated }
DartAssignmentOperatorNode >> compoundAssignmentOperator [

	^ compoundAssignmentOperator
]

{ #category : #generated }
DartAssignmentOperatorNode >> compoundAssignmentOperator: aSmaCCToken [

	compoundAssignmentOperator := aSmaCCToken
]

{ #category : #generated }
DartAssignmentOperatorNode >> equalsSymbol [

	^ equalsSymbol
]

{ #category : #generated }
DartAssignmentOperatorNode >> equalsSymbol: aSmaCCToken [

	equalsSymbol := aSmaCCToken
]

{ #category : #generated }
DartAssignmentOperatorNode >> tokenVariables [

	^ #( #equalsSymbol #compoundAssignmentOperator )
]
