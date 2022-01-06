use Mojo::File qw(path);
use Mojo::JSON qw(decode_json);
use Util::H2O;
use English;

use header -noclean;

my $dbh = DI->get('db')->db->dbh;

use constant CHUNK_SIZE => 1000;

sub create_insert ($table, @data)
{
	my @keys;
	my @values;

	for my $point (@data) {
		@keys = sort keys $point->%*
			unless @keys;

		my $value = join ',', map { $dbh->quote($point->{$_}) } @keys;
		push @values, "($value)";
	}

	my @chunks;
	push @chunks, [ splice @values, 0, CHUNK_SIZE ] while @values;

	local $LIST_SEPARATOR = ', ';

	my $sql = '';
	for my $chunk (@chunks) {
		@values = $chunk->@*;
		$sql .= <<~SQL;
			INSERT INTO $table (@keys) VALUES @values;
		SQL
	}

	return $sql;
}

sub create_translation_insert($type, @data)
{
	my $table = "gd_lore_${type}s";
	return create_insert $table, map {
		{ $_->%{qw(language_id lore_id), $type} }
	} @data;
}

sub store_dictionary($inserts, $object)
{
	push $inserts->dictionary->@*, {
		id => $object->{lore_id},
	};

	return;
}

sub store_translations($inserts, $object, $lang)
{
	push $inserts->translations->@*, {
		language_id => uc($lang),
		lore_id => $object->{lore_id},
		name => $object->{lore_name},
		description => $object->{lore_description},
	};

	return;
}

sub store_area ($inserts, $object, $lang)
{
	store_dictionary $inserts, $object;
	store_translations $inserts, $object, $lang;

	push $inserts->areas->@*, {
		id => $object->{lore_id},
	};

	return;
}

sub store_location ($inserts, $area_object, $object, $lang)
{
	store_dictionary $inserts, $object;
	store_translations $inserts, $object, $lang;

	push $inserts->locations->@*, {
		id => $object->{lore_id},
		area_id => $area_object->{lore_id},
		position_x => $object->{x} / $area_object->{canvas_width},
		position_y => $object->{y} / $area_object->{canvas_height},
	};

	return;
}

sub store_connection ($inserts, $array)
{
	push $inserts->connections->@*, {
		location_from_id => $array->[0],
		location_to_id => $array->[1],
	};

	return;
}

my $files = path->child('map_editor')->list({dir => 0});
my $inserts = h2o {
	dictionary => [],
	areas => [],
	locations => [],
	connections => [],
	translations => [],
};

for my $file ($files->each) {
	my $basename = $file->basename('.map.json');
	next unless $basename =~ m{ \A \w+ \. (\w{2}) \z }x;
	my $lang = $1;
	my $json = decode_json $file->slurp;

	store_area($inserts, $json, $lang);

	for my $location ($json->{markers}->@*) {
		store_location($inserts, $json, $location, $lang);
	}

	for my $connection ($json->{connections}->@*) {
		store_connection($inserts, $connection);
	}
}

my $contents = join "\n",
	create_insert(gd_lores =>  $inserts->dictionary->@*),
	create_insert(gd_areas =>  $inserts->areas->@*),
	create_insert(gd_locations =>  $inserts->locations->@*),
	create_insert(gd_location_paths =>  $inserts->connections->@*),
	create_translation_insert(name => $inserts->translations->@*),
	create_translation_insert(description => $inserts->translations->@*),
;

return <<SQL;

-- 7 up

$contents

-- 7 down

SQL
