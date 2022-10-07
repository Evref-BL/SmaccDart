Class {
	#name : #DartRethrowStatementNode,
	#superclass : #DartProgramNode,
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartRethrowStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitRethrowStatement: self
]
