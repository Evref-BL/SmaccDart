Class {
	#name : #DartStatementNode,
	#superclass : #DartProgramNode,
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitStatement: self
]
