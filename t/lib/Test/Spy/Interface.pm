package Test::Spy::Interface;

use My::Moose::Role;

use header;

requires qw(
	called_times
	call_history
	called_with
	first_called_with
	next_called_with
	last_called_with
	was_called
	was_called_once
	clear
);

