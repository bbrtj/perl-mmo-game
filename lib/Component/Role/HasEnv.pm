package Component::Role::HasEnv;

use Types;
use My::Moose::Role;

use header;

has 'env' => (
	is => 'ro',
	isa => Types::InstanceOf ['Component::Env'],
	required => 1,
);
