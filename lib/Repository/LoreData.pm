package Repository::LoreData;

use My::Moose;

use Utils;
use all 'X';

use header;

extends 'Repository';

my %named_collection;
my %collection;

sub save ($self, $obj)
{
	$named_collection{blessed $obj}{$obj->name} = $obj;
	$collection{$obj->id} = $obj;

	return;
}

sub check ($self, $id, $type = undef)
{
	return false unless defined $collection{$id};
	return true unless defined $type;
	return $collection{$id} isa 'Game::Lore::' . Utils->pascal_case($type);
}

sub load ($self, $id)
{
	my $found = $collection{$id};

	X::LoreNotFound->raise("no lore for $id identifier")
		unless defined $found;

	return $found;
}

sub load_named ($self, $class, $name)
{
	my $found = $named_collection{$class}{$name};

	X::LoreNotFound->raise("no lore for class $class and name $name")
		unless defined $found;

	return $found;
}

sub load_all ($self)
{
	return \%collection;
}

sub load_all_named ($self, $class)
{
	my $found = $named_collection{$class};

	X::LoreNotFound->raise("no lore collection for class $class")
		unless defined $found;

	return {map { $_->id => $_ } values $found->%*};
}

