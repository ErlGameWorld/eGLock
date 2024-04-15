eGLock
=====

    erlang的全局锁，不可重入。写这个的目的是考虑slg大地图场景的战斗会存在大乱斗，各个战斗目标血量处理需要全局锁处理。

Build
-----

    $ rebar3 compile

说明
----

    eALock 基于atmoics 
    eELock 基于Ets表
    eALock 更优