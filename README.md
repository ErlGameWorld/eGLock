eGLock
=====

    erlang的全局锁，不可重入。写这个的目的是考虑slg大地图场景的战斗会存在大乱斗，各个战斗目标血量处理需要全局锁处理。

Build
-----

    $ rebar3 compile

说明
----

    eGLock 基于c++11 atomic  
    其中要锁的key 不能是列表的字符串 原因是锁表的key会判断是否为list来区分是锁单key 还是锁列表
    另外建议锁的key需要通过宏定义来生成 维护时好搜查删改
    对于 tryLock和releaseLock的使用应该在try里面使用并在after调用releaseLock，就像这样:
    try 
        true = eGLock:tryLock(KeyOrKeys),
        %% doSomeThing
    catch C:R:S ->
        %% do error
    after 
        eGLock:releaseLock(KeyOrKeys)
    end.
