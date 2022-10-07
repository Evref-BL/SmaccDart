Class {
	#name : #DartThisExpressionNode,
	#superclass : #DartProgramNode,
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartThisExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitThisExpression: self
]
