-author("Hiroyuki Kato").
-vsn("0.0.1").

%% ======================================================================
%% Debug functions
%% ======================================================================

-define(app_debug, true).
-define(app_info, true).
-define(app_warn, true).
-define(app_error, true).

-ifdef(app_debug).
-define(APP_DEBUG(Str, Args), .erlang:apply(io, format, [.lists:concat([
  "[DEBUG] ", pid_to_list(self()), " ", ?MODULE, "/", ?LINE, " : ", Str, "~n"]), Args])).
-else.
-define(APP_DEBUG(Str, Args), ok).
-endif.

-ifdef(app_info).
-define(APP_INFO(Str, Args), .erlang:apply(io, format, [.lists:concat([
  "[INFO] ", pid_to_list(self()), " ", ?MODULE, "/", ?LINE, " : ", Str, "~n"]), Args])).
-else.
-define(APP_INFO(Str, Args), ok).
-endif.

-ifdef(app_warn).
-define(APP_WARN(Str, Args), .erlang:apply(io, format, [.lists:concat([
  "[WARN] ", pid_to_list(self()), " ", ?MODULE, "/", ?LINE, " : ", Str, "~n"]), Args])).
-else.
-define(APP_WARN(Str, Args), ok).
-endif.

-ifdef(app_error).
-define(APP_ERROR(Str, Args), .erlang:apply(io, format, [.lists:concat([
  "[ERROR] ", pid_to_list(self()), " ", ?MODULE, "/", ?LINE, " : ", Str, "~n"]), Args])).
-else.
-define(APP_ERROR(Str, Args), ok).
-endif.

