package Repository::LoreData;

use My::Moose;
use Exception::LoreNotFound;

use header;

with 'Repository::Role::Resource';

my %named_collection;
my %collection;

sub save ($self, $obj)
{
	$named_collection{blessed $obj}{$obj->name} = $obj;
	$collection{$obj->id} = $obj;

	return;
}

sub load ($self, $id)
{
	my $found = $collection{$id};

	Exception::LoreNotFound->throw(msg => "no lore for $id identifier")
		unless defined $found;

	return $found;
}

sub load_named ($self, $class, $name)
{
	my $found = $named_collection{$class}{$name};

	Exception::LoreNotFound->throw(msg => "no lore for class $class and name $name")
		unless defined $found;

	return $found;
}

sub load_all_named ($self, $class)
{
	my $found = $named_collection{$class};

	Exception::LoreNotFound->throw(msg => "no lore collection for class $class")
		unless defined $found;

	return $found;
}

sub dump ($self)
{
	use Data::Dumper;
	print Dumper(\%named_collection);
	return;
}

