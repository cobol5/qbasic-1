del windos.lib
del windos.qlb

lib windos.lib +intrpt+getput12+execute,nul;
link /q windos.lib,windos.qlb,nul,c:\program\qb71\library\qbxqlb.lib;
