package Game::LoreLoader::DSL;

use Game::LoreLoader;
use Game::LoreLoader::LoreDummy;
use Game::Config;
use DI;

use Sub::Util qw(set_subname);
use Mojo::File qw(path);
use Mojo::JSON qw(decode_json);

use header -noclean;

use constant TYPES => [qw(
	attribute
	race
	class
	item
	slot
	primary_stat
	secondary_stat
	area
	location
)];

use constant CONFIGS => [qw(
	translations
	define
	uses
	parent
)];

sub transform_name ($self, $name)
{
	$name = ucfirst $name;
	$name =~ s/_([a-z])/uc $1/eg;

	return $name;
}

sub get_helpers ($self)
{
	state $repo = DI->get('lore_data');
	my %subs;

	for my $type (TYPES->@*) {
		my $class = 'Game::Lore::' . $self->transform_name($type);

		$subs{"lore_$type"} = sub :prototype($) ($name) {
			return $repo->load_named($class, $name);
		};
	}

	return %subs;
}

sub _configure ($self, $context, $field, @values)
{
	my sub reporter ($text) {
		$context = $context ? ref $context : '(no context)';
		die sprintf $text, $context;
	}

	$context = $context->data
		unless $context->can($field);

	reporter "Context %s cannot utilize $field"
		unless $context->can($field);

	my $storage = $context->$field;

	if (is_hashref $storage) {
		if (@values == 1 && is_hashref $values[0]) {
			@values = $values[0]->%*;
		}

		my %kv_values = @values;
		for my $key (keys %kv_values) {
			reporter "replacing $key for $field in %s"
				if defined $storage->{$key};

			my $value = $kv_values{$key};
			$value = $value->create
				if $value->$_isa('Game::LoreLoader::LoreDummy');

			$storage->{$key} = $value;
		}
	}

	elsif (is_arrayref $storage) {
		for my $value (@values) {
			$value = $value->create
				if $value->$_isa('Game::LoreLoader::LoreDummy');

			push $storage->@*, $value;
		}
	}

	else {
		reporter "invalid data for plain $field for %s"
			unless @values == 1;

		my $value = $values[0];
		$value = $value->create
			if $value->$_isa('Game::LoreLoader::LoreDummy');

		my $setter = "set_$field";
		$context->$setter($value);
	}
}

sub get_dsl ($self, $caller)
{
	state $repo = DI->get('lore_data');
	my @items;

	my %dsl = (
		lore => sub ($id, $el) {
			my $class =  $el->class;
			eval "require $class";
			push @items, $class->new(
				id => $id,
				name => $el->name,
			);

			return;
		},
		requires => sub ($dependency) {
			Game::LoreLoader->load($dependency);
			return;
		},
		connection => sub ($from, $to) {
			$from = $from->create;
			$to = $to->create;

			push $from->data->connections->@*, $to;
			push $to->data->connections->@*, $from;
			return;
		},
		specify => sub ($what, @values) {
			my $context = $items[-1] // 'Game::Config';
			$self->_configure($context, $what, @values);
		},
		load_coordinates => sub ($from_key) {
			my $file = path($caller->FILENAME);
			my $json_file = $file->dirname->child($file->basename('.' . Game::LoreLoader->EXTENSION) . '.json');

			return unless -f $json_file;
			my $coordinates = decode_json $json_file->slurp;

			for my $item ($coordinates->{$from_key}->@*) {
				my ($lore_id, $x, $y) = $item->@{qw(LoreId PosX PosY)};
				my $lore_item = $repo->load($lore_id);
				$lore_item->data->set_pos_x($x);
				$lore_item->data->set_pos_y($y);
			}

			return;
		},
	);

	for my $type (TYPES->@*) {
		my $class = 'Game::Lore::' . $self->transform_name($type);

		$dsl{$type} = sub :prototype($) ($name) {
			return Game::LoreLoader::LoreDummy->new(
				class => $class,
				name => $name,
			);
		};
	}

	for my $config (CONFIGS->@*) {
		$dsl{$config} = sub (@values) {
			my $context = $items[-1] // 'Game::Config';
			$self->_configure($context, $config, @values);
		};
	}

	return %dsl;
}

sub import ($self, @args)
{
	my $package = caller;
	my $want_helpers = (grep { $_ eq -helpers } @args) > 0;
	my %subs = $want_helpers ? $self->get_helpers : $self->get_dsl($package);

	for my $name (keys %subs) {
		no strict 'refs';

		set_subname $name, $subs{$name};
		*{"${package}::${name}"} = $subs{$name};
	}

	return;
}

