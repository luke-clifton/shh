# Revision history for shh

## 0.3.X.X -- 2019-03-10

* Changes how lazy reading works. We no longer terminate the process, we
  just close the handles and wait for the process to terminate naturally.

  This eliminates a source of non-determinism

## 0.2.X.X -- 2019-01-23

* Extended the ExecArg typeclass to handle lists.

## 0.1.X.X  -- 2018-11-02

* First version. Released on an unsuspecting world.
