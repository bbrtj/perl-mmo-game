package CLI;

use My::Moose;
use Module::Load;
use Utils;

use header;

sub _build_command ($self, $name)
{
	my $full_name = "CLI::Command::$name";

	try {
		load $full_name;
	}
	catch ($e) {
		return undef;
	}

	return $full_name->new;
}

sub _build_all_commands ($self)
{
	my @classes = Utils->find_subclasses('CLI::Command');

	my %loaded;
	foreach my $class (@classes) {
		$loaded{$class} = $class->new;
	}

	return \%loaded;
}

sub help ($self, $command = undef)
{
	my $commands = $self->_build_all_commands;

	# TODO: sort
	foreach my ($class, $object) ($commands->%*) {
		next if defined $command && $command ne $object->command;

		$object->help(!defined $command);
	}

	return;
}

sub run_command ($self, $name, @args)
{
	return $self->help(@args) if $name eq 'help';

	my $command = $self->_build_command($name);
	croak "no such command: $name"
		unless $command;

	return $command->run(@args);
}

