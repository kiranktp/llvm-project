! REQUIRES: openmp_runtime

!  ! RUN: %python %S/../test_errors.py %s %flang_fc1 %openmp_flags %openmp_module_flag -fopenmp-version=52
! RUN: %flang_fc1 -fdebug-unparse -fopenmp -fopenmp-version=52 %s | FileCheck --ignore-case --check-prefix="UNPARSE" %s
! RUN: %flang_fc1 -fdebug-dump-parse-tree -fopenmp -fopenmp-version=52 %s | FileCheck --check-prefix="PARSE-TREE" %s

use omp_lib

  integer :: val = 10, j , k, omp_cur_iteration

  !$OMP DO ORDERED
  do i = 1,val
    if (i <= 3) then
  !$OMP ORDERED doacross(sink: j-1)
      k = k + 1
  !$OMP END ORDERED
    endif
    if (i <= 6) then
  !$OMP ORDERED doacross(source : omp_cur_iteration)
      k = k + 1
  !$OMP END ORDERED
    endif
  enddo 
end program

!UNPARSE:  USE :: omp_lib
!UNPARSE:  INTEGER :: val = 10_4, j, k, omp_cur_iteration
!UNPARSE: !$OMP DO  ORDERED
!UNPARSE:  DO i=1_4,val
!UNPARSE:   IF (i<=3_4) THEN
!UNPARSE: !$OMP ORDERED  DOACROSS(SINK:j-1_4))
!UNPARSE:     k=k+1_4
!UNPARSE: !$OMP END ORDERED 
!UNPARSE:   END IF
!UNPARSE:   IF (i<=6_4) THEN
!UNPARSE: !$OMP ORDERED  DOACROSS(SOURCE:omp_cur_iteration))
!UNPARSE:     k=k+1_4
!UNPARSE: !$OMP END ORDERED 
!UNPARSE:   END IF
!UNPARSE:  END DO
!UNPARSE: END PROGRAM

!PARSE-TREE: Program -> ProgramUnit -> MainProgram
!PARSE-TREE: | SpecificationPart
!PARSE-TREE: | | UseStmt
!PARSE-TREE: | | | Name = 'omp_lib'
!PARSE-TREE: | | ImplicitPart -> 
!PARSE-TREE: | | DeclarationConstruct -> SpecificationConstruct -> TypeDeclarationStmt
!PARSE-TREE: | | | DeclarationTypeSpec -> IntrinsicTypeSpec -> IntegerTypeSpec -> 
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'val'
!PARSE-TREE: | | | | Initialization -> Constant -> Expr = '10_4'
!PARSE-TREE: | | | | | LiteralConstant -> IntLiteralConstant = '10'
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'j'
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'k'
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'omp_cur_iteration'
!PARSE-TREE: | ExecutionPart -> Block
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPLoopConstruct
!PARSE-TREE: | | | OmpBeginLoopDirective
!PARSE-TREE: | | | | OmpLoopDirective -> llvm::omp::Directive = do
!PARSE-TREE: | | | | OmpClauseList -> OmpClause -> Ordered -> 
!PARSE-TREE: | | | DoConstruct
!PARSE-TREE: | | | | NonLabelDoStmt
!PARSE-TREE: | | | | | LoopControl -> LoopBounds
!PARSE-TREE: | | | | | | Scalar -> Name = 'i'
!PARSE-TREE: | | | | | | Scalar -> Expr = '1_4'
!PARSE-TREE: | | | | | | | LiteralConstant -> IntLiteralConstant = '1'
!PARSE-TREE: | | | | | | Scalar -> Expr = 'val'
!PARSE-TREE: | | | | | | | Designator -> DataRef -> Name = 'val'
!PARSE-TREE: | | | | Block
!PARSE-TREE: | | | | | ExecutionPartConstruct -> ExecutableConstruct -> IfConstruct
!PARSE-TREE: | | | | | | IfThenStmt
!PARSE-TREE: | | | | | | | Scalar -> Logical -> Expr = 'i<=3_4'
!PARSE-TREE: | | | | | | | | LE
!PARSE-TREE: | | | | | | | | | Expr = 'i'
!PARSE-TREE: | | | | | | | | | | Designator -> DataRef -> Name = 'i'
!PARSE-TREE: | | | | | | | | | Expr = '3_4'
!PARSE-TREE: | | | | | | | | | | LiteralConstant -> IntLiteralConstant = '3'
!PARSE-TREE: | | | | | | Block
!PARSE-TREE: | | | | | | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPBlockConstruct
!PARSE-TREE: | | | | | | | | OmpBeginBlockDirective
!PARSE-TREE: | | | | | | | | | OmpBlockDirective -> llvm::omp::Directive = ordered
!PARSE-TREE: | | | | | | | | | OmpClauseList -> OmpClause -> Doacross -> OmpDoacrossClause -> DoacrossSink -> OmpDoacrossSinkVec
!PARSE-TREE: | | | | | | | | | | Name = 'j'
!PARSE-TREE: | | | | | | | | | | OmpDoacrossSinkVecLength
!PARSE-TREE: | | | | | | | | | | | DefinedOperator -> IntrinsicOperator = Subtract
!PARSE-TREE: | | | | | | | | | | | Scalar -> Integer -> Constant -> Expr = '1_4'
!PARSE-TREE: | | | | | | | | | | | | LiteralConstant -> IntLiteralConstant = '1'
!PARSE-TREE: | | | | | | | | Block
!PARSE-TREE: | | | | | | | | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> AssignmentStmt = 'k=k+1_4'
!PARSE-TREE: | | | | | | | | | | Variable = 'k'
!PARSE-TREE: | | | | | | | | | | | Designator -> DataRef -> Name = 'k'
!PARSE-TREE: | | | | | | | | | | Expr = 'k+1_4'
!PARSE-TREE: | | | | | | | | | | | Add
!PARSE-TREE: | | | | | | | | | | | | Expr = 'k'
!PARSE-TREE: | | | | | | | | | | | | | Designator -> DataRef -> Name = 'k'
!PARSE-TREE: | | | | | | | | | | | | Expr = '1_4'
!PARSE-TREE: | | | | | | | | | | | | | LiteralConstant -> IntLiteralConstant = '1'
!PARSE-TREE: | | | | | | | | OmpEndBlockDirective
!PARSE-TREE: | | | | | | | | | OmpBlockDirective -> llvm::omp::Directive = ordered
!PARSE-TREE: | | | | | | | | | OmpClauseList -> 
!PARSE-TREE: | | | | | | EndIfStmt -> 
!PARSE-TREE: | | | | | ExecutionPartConstruct -> ExecutableConstruct -> IfConstruct
!PARSE-TREE: | | | | | | IfThenStmt
!PARSE-TREE: | | | | | | | Scalar -> Logical -> Expr = 'i<=6_4'
!PARSE-TREE: | | | | | | | | LE
!PARSE-TREE: | | | | | | | | | Expr = 'i'
!PARSE-TREE: | | | | | | | | | | Designator -> DataRef -> Name = 'i'
!PARSE-TREE: | | | | | | | | | Expr = '6_4'
!PARSE-TREE: | | | | | | | | | | LiteralConstant -> IntLiteralConstant = '6'
!PARSE-TREE: | | | | | | Block
!PARSE-TREE: | | | | | | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPBlockConstruct
!PARSE-TREE: | | | | | | | | OmpBeginBlockDirective
!PARSE-TREE: | | | | | | | | | OmpBlockDirective -> llvm::omp::Directive = ordered
!PARSE-TREE: | | | | | | | | | OmpClauseList -> OmpClause -> Doacross -> OmpDoacrossClause -> DoacrossSource -> OmpDoacrossSinkVec
!PARSE-TREE: | | | | | | | | | | Name = 'omp_cur_iteration'
!PARSE-TREE: | | | | | | | | Block
!PARSE-TREE: | | | | | | | | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> AssignmentStmt = 'k=k+1_4'
!PARSE-TREE: | | | | | | | | | | Variable = 'k'
!PARSE-TREE: | | | | | | | | | | | Designator -> DataRef -> Name = 'k'
!PARSE-TREE: | | | | | | | | | | Expr = 'k+1_4'
!PARSE-TREE: | | | | | | | | | | | Add
!PARSE-TREE: | | | | | | | | | | | | Expr = 'k'
!PARSE-TREE: | | | | | | | | | | | | | Designator -> DataRef -> Name = 'k'
!PARSE-TREE: | | | | | | | | | | | | Expr = '1_4'
!PARSE-TREE: | | | | | | | | | | | | | LiteralConstant -> IntLiteralConstant = '1'
!PARSE-TREE: | | | | | | | | OmpEndBlockDirective
!PARSE-TREE: | | | | | | | | | OmpBlockDirective -> llvm::omp::Directive = ordered
!PARSE-TREE: | | | | | | | | | OmpClauseList -> 
!PARSE-TREE: | | | | | | EndIfStmt -> 
!PARSE-TREE: | | | | EndDoStmt -> 
!PARSE-TREE: | EndProgramStmt -> 

