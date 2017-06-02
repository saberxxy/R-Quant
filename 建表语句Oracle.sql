create table relevance
(uuid varchar2(50) primary key,
code varchar2(6),
name varchar2(80),
type varchar2(10),
tablename varchar2(50),
ca number(5, 4), 
corpearson number(5, 4), 
corspearman number(5, 4), 
corkendall number(5, 4)
);


comment on table relevance is '指数与个股相关性表';
comment on column relevance.uuid is '主键';
comment on column relevance.code is '股票代码';
comment on column relevance.name is '股票名';
comment on column relevance.type is '相关性分析的数据组';
comment on column relevance.tablename is '表名';
comment on column relevance.ca is '典型相关性';
comment on column relevance.corpearson is '皮尔逊相关系数';
comment on column relevance.corspearman is '秩相关系数';
comment on column relevance.corkendall is '和谐系数';

