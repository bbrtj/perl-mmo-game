package Component::Role::HasEnv;

use My::Moose::Role;

use header;

has param 'env' => (
	isa => Types::InstanceOf ['Component::Env'],
);

