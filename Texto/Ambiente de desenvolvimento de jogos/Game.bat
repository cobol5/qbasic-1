@echo off
intro
kaboom boo1.map
if not exist booalive.dat goto dead
kaboom boo2.map
if not exist booalive.dat goto dead
kaboom boo3.map
if not exist booalive.dat goto dead
finish
:dead
