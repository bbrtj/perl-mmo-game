package Game::LoreLoader;

use Path::Tiny;
use Encode qw(decode);
use JSON::MaybeXS qw(decode_json);
use XML::PugiXML;
use Game::Config;
use Sub::Install;
use Value::Diff;
use Utils;

use all 'Game::Lore';

use header;

use constant LORE_TYPES => [
	qw(
		alliance
		class race ability npc
		primary_stat secondary_stat attribute
		area location
		item slot
	)
];

use constant DIRECTORY => path(__FILE__)->parent->parent->parent->child('game-data');
use constant EXTENSION => 'xml';

DI->static_injected('lore_data_repo');

our $LORE_FILENAME;

my sub build_class ($name)
{
	return 'Game::Lore::' . Utils->pascal_case($name);
}

my sub real_children ($item)
{
	return grep { $_->type == XML::PugiXML::NODE_ELEMENT } $item->children;
}

my sub get_lore_from_attr ($item)
{
	my $lore = $item->attr('lore');
	return undef unless $lore;
	my $type = $lore->value;

	my $lore_key = $item->attr('lore-key');
	my $name = $lore_key ? $lore_key->value : $item->text;

	return __PACKAGE__->lore_data_repo->load_named(build_class($type), $name);
}

my sub is_lore_key ($item)
{
	return defined $item->attr('lore-key');
}

my sub structure_or_text ($item)
{
	my @children = $item->&real_children;
	if (@children || $item->attr('empty_structure')) {
		my %hash;
		foreach my $child (@children) {
			my $lore = $child->&get_lore_from_attr;
			my $name = $child->&is_lore_key ? $child->&get_lore_from_attr->id : $child->name;

			$hash{$name} = __SUB__->($child);
		}

		return \%hash;
	}
	else {
		my $lore_value = $item->&get_lore_from_attr;
		return $lore_value
			if $lore_value && !$item->&is_lore_key;

		return $item->text;
	}
}

sub is_lore_item ($self, $item)
{
	my $name = $item->name;
	return any { $name eq $_ } LORE_TYPES->@*;
}

sub build_config ($self, $node)
{
	foreach my $constant ($node->&real_children) {
		my $value = $constant->&structure_or_text;

		Sub::Install::install_sub(
			{
				code => sub { $value },
				as => $constant->name,
				into => 'Game::Config',
			}
		);
	}

	return;
}

sub build_single_lore ($self, $item, $parent = undef)
{
	my $id = $item->attr('id')->value;
	my $lore_name = $item->attr('name')->value;

	my %translations;
	my %data;
	my @uses;
	my %other_args;
	my %coordinates;
	my @children;

	foreach my $item_data ($item->&real_children) {
		my $name = $item_data->name;

		if ($name eq 'translation') {
			$translations{$item_data->attr('lang')->value} = $item_data->&structure_or_text;
		}
		elsif ($name eq 'uses') {
			push @uses, $item_data->&get_lore_from_attr
				// die "no lore for 'uses' node";
		}
		elsif ($self->is_lore_item($item_data)) {
			push @children, $item_data;
		}
		elsif ($name eq 'load_coordinates') {
			my $from_key = $item_data->text;
			my $file = path($LORE_FILENAME);
			my $json_file = $file->parent->child($file->basename('.' . EXTENSION) . '.json');

			die "bad json file to load coordinates from: $json_file"
				unless -e $json_file;
			my $json_data = decode_json $json_file->slurp;

			foreach my $item ($json_data->{$from_key}->@*) {
				my ($lore_id, $x, $y) = $item->@{qw(LoreId PosX PosY)};
				$coordinates{$lore_id} = [$x, $y];
			}
		}
		else {
			$other_args{$name} = $item_data->&structure_or_text;
		}
	}

	my $class = build_class($item->name);
	my $built = $class->new(
		%other_args,
		id => $id,
		name => $lore_name,
		translations => \%translations,
		uses => \@uses,
		($parent ? (parent => $parent) : ()),
	);

	foreach my $child (@children) {
		$self->build_single_lore($child, $built);
	}

	foreach my ($lore_id, $position) (%coordinates) {
		my $lore_item = $self->lore_data_repo->load($lore_id);
		$lore_item->set_pos_x($position->[0]);
		$lore_item->set_pos_y($position->[1]);
	}

	return;
}

sub build_lore ($self, $type, $node)
{
	return $self->build_config($node)
		if $type eq 'constants';

	foreach my $item ($node->&real_children) {
		$self->build_single_lore($item);
	}

	return;
}

sub load_xml ($self, $name)
{
	$name =~ s/\.@{[EXTENSION]}$//i;

	my $filename = DIRECTORY->child("$name." . EXTENSION);
	die "$filename does not exist"
		unless -e $filename;

	my $parser = XML::PugiXML->new;
	$parser->load_string(decode 'utf-8', $filename->slurp);

	my @to_build;
	my $item = $parser->root;
	my $requires = $item->attr('requires');
	push @to_build, [
		$item,
		$item->name,
		$filename,
		$requires ? [map { trim $_ } split /,/, $requires->value] : [],
	];

	return @to_build;
}

sub sort_lore ($self, @to_build)
{
	my %built_lore;
	my @build_order;

	while (@to_build) {
		my $found;
		foreach my ($key, $item) (indexed @to_build) {
			next if diff($item->[3], [keys %built_lore]);
			push @build_order, $item;
			$built_lore{$item->[1]} = true;
			$found = $key;
			last;
		}

		if (defined $found) {
			splice @to_build, $found, 1;
		}
		else {
			die 'lore deadlock: could not find any more lore to add. Lores left: ' . My::Dumper->dd(\@to_build);
		}
	}

	return @build_order;
}

sub load_all ($self)
{
	state $loaded = false;
	return if $loaded;

	my $iter = DIRECTORY->iterator({recurse => true});
	my $ext_re = qr{\.@{[EXTENSION]}$}i;

	my @to_build;
	while (my $file = $iter->()) {
		next unless $file =~ $ext_re;
		push @to_build, $self->load_xml($file->relative(DIRECTORY));
	}

	foreach my $item_data ($self->sort_lore(@to_build)) {
		local $LORE_FILENAME = $item_data->[2];
		$self->build_lore($item_data->[1], $item_data->[0]);
	}

	$loaded = true;
	return;
}

sub import ($self)
{
	$self->load_all;

	return;
}

