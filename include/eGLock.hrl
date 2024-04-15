%% 锁key
-define(EtsGLockKey, '$EtsGLockKey').

%% 默认超时时间单位:Ms
-define(LockTimeOut, 5000).

%% 超时重试时间单位:Ms
-define(ReTryTime, 10).

-define(eELockMgr, eELockMgr).

%% 数组数量
-define(eALockSize, 1048576).
%% atomics索引
-define(eALockRef, eALockRef).
