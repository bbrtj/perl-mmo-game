package Repository::LoreData;

use My::Moose;

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
	return !!0 unless defined $collection{$id};
	return !!1 unless defined $type;
	return $collection{$id} isa 'Game::Lore::' . ucfirst $type;
}

sub load ($self, $id)
{
	my $found = $collection{$id};

	X::LoreNotFound->throw("no lore for $id identifier")
		unless defined $found;

	return $found;
}

sub load_named ($self, $class, $name)
{
	my $found = $named_collection{$class}{$name};

	X::LoreNotFound->throw("no lore for class $class and name $name")
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

	X::LoreNotFound->throw("no lore collection for class $class")
		unless defined $found;

	return $found;
}

