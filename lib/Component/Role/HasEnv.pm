package Component::Role::HasEnv;

use My::Moose::Role;

use header;

has param 'env' => (
	isa => InstanceOf ['Component::Env'],
);

