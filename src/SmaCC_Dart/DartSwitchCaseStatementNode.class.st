Class {
	#name : #DartSwitchCaseStatementNode,
	#superclass : #DartStatementNode,
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSwitchCaseStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSwitchCaseStatement: self
]
