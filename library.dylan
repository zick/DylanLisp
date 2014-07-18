Module: dylan-user

define library dylanlisp
  use common-dylan;
  use io;
end library;

define module dylanlisp
  use common-dylan, exclude: { format-to-string };
  use format-out;
  use standard-io;
  use streams;
end module;
