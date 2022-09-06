Trait {
	#name : #TDartProgramNodeVisitor,
	#traits : 'TSmaCCParseNodeVisitor',
	#classTraits : 'TSmaCCParseNodeVisitor classTrait',
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
TDartProgramNodeVisitor >> visitAssertStatement: anAssertStatement [

	^ self visitStatement: anAssertStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitAsyncFunctionBody: anAsyncFunctionBody [

	^ self visitFunctionBody: anAsyncFunctionBody
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitBlock: aBlock [

	^ self visitProgram: aBlock
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitBreakStatement: aBreakStatement [

	^ self visitStatement: aBreakStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitClassDeclaration: aClassDeclaration [

	^ self visitProgram: aClassDeclaration
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitClassMemberDefinition: aClassMemberDefinition [

	^ self visitProgram: aClassMemberDefinition
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitContinueStatement: aContinueStatement [

	^ self visitStatement: aContinueStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitDoStatement: aDoStatement [

	^ self visitStatement: aDoStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitExportDeclaration: anExportDeclaration [

	^ self visitImportExportDeclaration: anExportDeclaration
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitExpressionStatement: anExpressionStatement [

	^ self visitStatement: anExpressionStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitForStatement: aForStatement [

	^ self visitStatement: aForStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitFunctionBody: aFunctionBody [

	^ self visitProgram: aFunctionBody
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitIfStatement: anIfStatement [

	^ self visitStatement: anIfStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitImportDeclaration: anImportDeclaration [

	^ self visitImportExportDeclaration: anImportDeclaration
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitImportExportDeclaration: anImportExportDeclaration [

	^ self visitProgram: anImportExportDeclaration
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitLibraryDefinition: aLibraryDefinition [

	^ self visitProgram: aLibraryDefinition
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitLocalVariableDeclaration: aLocalVariableDeclaration [

	^ self visitStatement: aLocalVariableDeclaration
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitProgram: aProgram [

	^ self visitSmaCCParseNode: aProgram
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitReturnStatement: aReturnStatement [

	^ self visitStatement: aReturnStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitStatement: aStatement [

	^ self visitProgram: aStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitSwitchCaseStatement: aSwitchCaseStatement [

	^ self visitStatement: aSwitchCaseStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitSwitchStatement: aSwitchStatement [

	^ self visitProgram: aSwitchStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitTryStatement: aTryStatement [

	^ self visitStatement: aTryStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitType: aType [

	^ self visitProgram: aType
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitWhileStatement: aWhileStatement [

	^ self visitStatement: aWhileStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitYieldEachStatement: aYieldEachStatement [

	^ self visitYieldStatement: aYieldEachStatement
]

{ #category : #generated }
TDartProgramNodeVisitor >> visitYieldStatement: aYieldStatement [

	^ self visitStatement: aYieldStatement
]
