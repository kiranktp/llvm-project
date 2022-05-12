! RUN: %flang_fc1 -fdebug-unparse -fopenmp %s | FileCheck --ignore-case %s
! RUN: %flang_fc1 -fdebug-dump-parse-tree -fopenmp %s | FileCheck --check-prefix="PARSE-TREE" %s

subroutine openmp_task(n, obj)
  use omp_lib
  integer, intent(in) :: n
  integer ::a, b
  integer :: v(n)
  integer(omp_depend_kind) :: obj

  !$omp parallel
  !$omp single
!==============================================================================
! Task with depend(in.. )
!==============================================================================
!CHECK: !$OMP TASK  DEPEND((IN:a))
  !$omp task depend(in: a)
    b = b + a
!CHECK: !$OMP END TASK
  !$omp end task

!PARSE-TREE: OmpBeginBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task
!PARSE-TREE: OmpClauseList -> OmpClause -> Depend -> OmpDependClause -> IterDepTypeList
!PARSE-TREE: OmpDependenceType -> Type = In
!PARSE-TREE: OmpEndBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task

!==============================================================================
! Task with depend(out.. )
!==============================================================================
!CHECK: !$OMP TASK  DEPEND((OUT:a))
  !$omp task depend(out: a)
    a = b + a
!CHECK: !$OMP END TASK
  !$omp end task

!PARSE-TREE: OmpBeginBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task
!PARSE-TREE: OmpClauseList -> OmpClause -> Depend -> OmpDependClause -> IterDepTypeList
!PARSE-TREE: OmpDependenceType -> Type = Out
!PARSE-TREE: OmpEndBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task

!==============================================================================
! Task with depend(inout.. )
!==============================================================================
!CHECK: !$OMP TASK  DEPEND((INOUT:a))
  !$omp task depend(inout: a)
    a = b + a
!CHECK: !$OMP END TASK
  !$omp end task

!PARSE-TREE: OmpBeginBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task
!PARSE-TREE: OmpClauseList -> OmpClause -> Depend -> OmpDependClause -> IterDepTypeList
!PARSE-TREE: OmpDependenceType -> Type = Inout
!PARSE-TREE: OmpEndBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task

!==============================================================================
! Task with depend(mutexinoutset.. )
!==============================================================================
!CHECK: !$OMP TASK  DEPEND((IN:a)) DEPEND((MUTEXINOUTSET:b))
  !$omp task depend(in: a) depend(mutexinoutset: b)
    b = b + a
!CHECK: !$OMP END TASK
  !$omp end task

!PARSE-TREE: OmpBeginBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task
!PARSE-TREE: OmpClauseList -> OmpClause -> Depend -> OmpDependClause -> IterDepTypeList
!PARSE-TREE: OmpDependenceType -> Type = In
!PARSE-TREE: OmpDependenceType -> Type = Mutexinoutset
!PARSE-TREE: OmpEndBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task

!==============================================================================
! Task with Iterator
!==============================================================================
!CHECK: !$OMP TASK  DEPEND((ITERATOR( it=1_4:n), IN:v(it)))
  !$omp task depend(iterator(it = 1:n), in: v(it))
!CHECK: CALL print_val(v,n)
  call print_val(v, n)
!CHECK: !$OMP END TASK
  !$omp end task

!PARSE-TREE: OmpBeginBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task
!PARSE-TREE: OmpClauseList -> OmpClause -> Depend -> OmpDependClause -> IterDepTypeList
!PARSE-TREE: OmpIterator
!PARSE-TREE: OmpIteratorSpecifierList -> OmpIteratorSpecifier
!PARSE-TREE: LoopBounds
!PARSE-TREE: OmpDependenceType -> Type = In
!PARSE-TREE: OmpEndBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task

!==============================================================================
! Task with depend(depobj..)
!==============================================================================
!CHECK: !$OMP TASK  DEPEND((DEPOBJ:obj))
  !$omp task depend(depobj: obj)
!CHECK: CALL f1(a,b,n)
  call F1(a, b, n)
!CHECK: !$OMP END TASK
  !$omp end task

!PARSE-TREE: OmpBeginBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task
!PARSE-TREE: OmpClauseList -> OmpClause -> Depend -> OmpDependClause -> IterDepTypeList
!PARSE-TREE: OmpDependenceType -> Type = Depobj
!PARSE-TREE: OmpEndBlockDirective
!PARSE-TREE: OmpBlockDirective -> llvm::omp::Directive = task

  !$omp end single
  !$omp end parallel

END subroutine openmp_task
