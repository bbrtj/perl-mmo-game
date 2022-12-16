package CLI::translate;

use My::Moose -constr;
use Getopt::Long qw(GetOptionsFromArray);
use Mojo::File qw(path);
use Text::Levenshtein::BV;
use YAML::Tiny;
use CLI::export_mo;

use header;

extends 'Mojolicious::Command';

use constant description => 'translate text for the application';
sub usage ($self) { return $self->extract_usage }

has field 'languages' => (
	default => sub { {} },
);

has field 'translations' => (
	lazy => 1,
);

binmode STDOUT, ':encoding(UTF-8)';
binmode STDIN, ':encoding(UTF-8)';

sub _build_translations ($self)
{
	my @files = $self->_get_translation_files;
	my @langs = map { /(..)\.ya?ml$/ } @files;

	return [
		map {
			my $arr = YAML::Tiny->read($_);
			$self->languages->{$arr} = shift @langs;
			$arr;
		} @files
	];
}

sub _prompt ($self, $text = undef)
{
	if ($text) {
		print $text;
		print ':' unless $text =~ /[:>?]$/;
		print ' ';
	}

	my $answer = readline STDIN;
	chomp $answer;

	return $answer;
}

sub _sync ($self)
{
	for my $trans ($self->translations->@*) {
		my $language = $self->languages->{$trans};
		my $filename = path->child('i18n')->child("$language.yml");

		$trans->@* = sort { $a->{id} cmp $b->{id} } $trans->@*;

		$trans->write($filename);
	}

	return;
}

sub _get_translation_files ($self)
{
	return glob path->child('i18n')->child('*.yml');
}

sub _calculate_differences ($self)
{
	my @translations = $self->translations->@*;
	my @masks = map { 1 << $_ } keys @translations;
	my %found;

	foreach my $key (keys @translations) {
		my ($trans, $mask) = ($translations[$key], $masks[$key]);

		foreach my $el ($trans->@*) {
			$found{$el->{id}} //= 0;
			$found{$el->{id}} |= $mask
				unless length $el->{str} == 0;
		}
	}

	my @diffs;
	foreach my $key (keys %found) {
		my $value = $found{$key};

		my @gathered_langs;
		foreach my $mask_key (keys @masks) {
			push @gathered_langs, $self->languages->{$translations[$mask_key]}
				if $value & $masks[$mask_key];
		}

		push @diffs, [$key, @gathered_langs]
			unless @gathered_langs == @translations;
	}

	return @diffs;
}

sub _find_name_suggestions ($self, $trans, $key, $max_distance = 3)
{
	my $LS = Text::Levenshtein::BV->new;
	my $min_distance = $max_distance;
	my @min_matches;

	my $search = [split '', $key];
	foreach my $el ($trans->@*) {
		my $id = $el->{id};
		my $distance = $LS->distance($search, [split '', $id]);

		if ($distance <= $min_distance) {
			$min_distance = $distance;
			push @min_matches, $id;
		}
	}

	return @min_matches;
}

sub _print_name_suggestions ($self, $trans, $key, $max_distance = 3)
{
	my @matches = $self->_find_name_suggestions($trans, $key, $max_distance);

	say "Oops, could't find that!";
	if (@matches) {
		my $suggestions = join ' or ', map { qq{"$_"} } @matches;
		say "Maybe you meant $suggestions?";
	}

	return;
}

sub _find_by_id ($self, $key)
{
	my @found;
	foreach my $trans ($self->translations->@*) {
		foreach my $el (keys $trans->@*) {
			next unless $trans->[$el]{id} eq $key;
			push @found, [$trans, $el];
		}
	}

	return @found;
}

sub run_translate ($self, $key)
{
	foreach my $trans ($self->translations->@*) {
		say "Translating for " . $self->languages->{$trans};

		my $found = first { $_->{id} eq $key } $trans->@*;
		if ($found) {
			say "It now says: " . $found->{str};
			say "(Leave empty to keep it)";
		}

		my $new = $self->_prompt('Translation');

		die 'No text specified and it did not exist, aborting'
			if !length $new && !$found;

		if ($found) {
			$found->{str} = $new
				if length $new;
		}
		else {
			push $trans->@*, {
				id => $key,
				str => $new
			};
		}
	}

	$self->_sync;

	return;
}

sub run_rename ($self, $key_from, $key_to)
{
	my @found = $self->_find_by_id($key_from);
	my @found_to = $self->_find_by_id($key_to);

	foreach my $data (@found) {
		foreach my $data_to (@found_to) {
			die "key $key_to already exists!"
				if $data->[0] eq $data_to->[0];
		}
	}

	if (@found == 0) {
		$self->_print_name_suggestions($self->translations->[0], $key_from, 4);
	}
	else {
		foreach my $data (@found) {
			my ($trans, $el) = @$data;
			push $trans->@*, {
				id => $key_to,
				str => $trans->[$el]{str},
			};

			splice $trans->@*, $el, 1;
		}
	}

	$self->_sync;

	say 'Renamed elements: ' . scalar @found;

	return;
}

sub run_fix ($self)
{
	my @diffs = $self->_calculate_differences;

	while (@diffs) {
		my @new_diffs;
		foreach my $arr (sort { $a->[0] cmp $b->[0] } @diffs) {
			my ($id, @langs) = $arr->@*;

			say "$id - present in " . join ', ', @langs;
			say "Options:";
			say "t. Translate this string";
			say "r. Rename this string";
			say "s. Do nothing";
			say "o. Do nothing yet, show others";

			my $choice = $self->_prompt('Choice');
			if ($choice eq 't') {
				$self->run_translate($id);
			}
			elsif ($choice eq 'r') {
				my $to = $self->_prompt('New key');
				$self->run_rename($id, $to);
			}
			elsif ($choice eq 's') {

				# do nothing
			}
			elsif ($choice eq 'o') {
				push @new_diffs, $arr;
			}
			else {
				say "Unknown option - lets start again";
				redo;
			}
		}
		@diffs = @new_diffs;
	}

	return;
}

sub run_show ($self, $key)
{
	my @found = $self->_find_by_id($key);

	if (@found == 0) {
		$self->_print_name_suggestions($self->translations->[0], $key, 4);
	}
	else {
		foreach my $data (sort { $self->languages->{$a->[0]} cmp $self->languages->{$b->[0]} } @found) {
			my ($trans, $el) = @$data;

			my $lang = $self->languages->{$trans};
			my $str = $trans->[$el]{str};
			say "$lang: $str";
		}
	}

	return;
}

sub run_search ($self, $query)
{
	$query = quotemeta $query;
	my $re = qr/$query/i;

	my %found;
	foreach my $trans ($self->translations->@*) {
		foreach my $el ($trans->@*) {
			next unless $el->{id} =~ $re;
			$found{$el->{id}} = 1;
		}
	}

	foreach my $id (keys %found) {
		say $id;
	}

	return;
}

sub keep_running ($self)
{
	say 'Entering translations loop, q or C-C to exit';

	while (-loop) {
		my @args = split /\s+/, $self->_prompt('trans>');

		last if @args == 1 && $args[0] eq 'q';
		$self->run(@args);
	}

	return;
}

sub run ($self, @args)
{
	if (@args == 1 && $args[0] eq 'loop') {
		$self->keep_running;
		return;
	}

	my $show = undef;
	my $search = undef;
	my $translate = undef;
	my $rename = undef;
	my $fix = 0;
	my $export = undef;

	GetOptionsFromArray(
		\@args,
		'show|s=s' => \$show,
		'list|l=s' => \$search,
		'translate|t=s' => \$translate,
		'rename|r=s' => \$rename,
		'fix|f' => \$fix,
		'export|e=s' => \$export,
	);

	if ($rename && $translate) {
		$self->run_rename($rename, $translate);
	}

	elsif ($translate) {
		$self->run_translate($translate);
	}


	elsif ($fix) {
		$self->run_fix();
	}

	elsif ($show) {
		$self->run_show($show);
	}

	elsif ($search) {
		$self->run_search($search);
	}

	elsif ($export) {
		CLI::export_mo->new->run($export);
	}

	else {
		$self->help;
	}

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION translate [loop or OPTIONS]
	Options:
		-s=[ID], --show [ID]  shows message strings for key ID
		-l=[QUERY], --list [QUERY]  Searches for given QUERY (substring of ID)
		-t=[ID], --translate [ID]  add or replace existing key
		-r=[ID] -t=[ID], --rename [ID] --translate [ID]  rename translation
		-f, --fix  prompts the user to fix all the missing translations
		-e=[LANG], --export [LANG]  exports mo out for the client

