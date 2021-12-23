package Web::Controller::API::Socket;

use Moo;
use Web::Config;

use header;

extends 'Mojolicious::Controller';

sub dispatch_message ($self, $msg)
{
	$self->send("echo: $msg");
}

sub websocket ($self)
{
	$self->on(message => \&dispatch_message);
}

