%% 锁key
-define(EtsGLockKey, '$EtsGLockKey').

%% 锁key
-define(EtsGLockPid, '$EtsGLockPid').

%% 默认超时时间单位:Ms
-define(LockTimeOut, 5000).

%% 超时重试时间单位:Ms
-define(ReTryTime, 10).

-define(eELockMgr, eELockMgr).
-define(eALockMgr, eALockMgr).

%% 数组数量
-define(eALockSize, 1048576).
%% 数组数量
-define(eALockRef, eALockRef).
