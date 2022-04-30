package CLI::translate;

use My::Moose -constr;
use Getopt::Long qw(GetOptionsFromArray);
use Mojo::File qw(path);
use Text::Levenshtein::BV;

use header;

extends 'Mojolicious::Command';

use constant description => 'translate text for the application';
sub usage ($self) { return $self->extract_usage }

has 'translations' => (
	is => 'ro',
	lazy => 1,
	default => sub ($self) {
		return [
			map {
				my $obj = Text::PO->new;
				$obj->parse($_);
				$obj
			} $self->_get_translation_files
		];
	},
);

sub _prompt ($self, $text = undef)
{
	if ($text) {
		print "$text: ";
	}

	my $answer = readline STDIN;
	chomp $answer;

	return $answer;
}

sub _get_translation_files ($self)
{
	return glob path->child('i18n')->child('*.po');
}

sub _calculate_differences ($self)
{
	my @translations = $self->translations->@*;
	my @masks = map { 1 << $_ } keys @translations;
	my %found;

	for my $key (keys @translations) {
		my ($trans, $mask) = ($translations[$key], $masks[$key]);

		for my $el ($trans->elements->list) {
			$found{$el->id} //= 0;
			$found{$el->id} |= $mask
				unless length $el->msgstr == 0;
		}
	}

	my @diffs;
	for my $key (keys %found) {
		my $value = $found{$key};

		my @gathered_langs;
		for my $mask_key (keys @masks) {
			push @gathered_langs, $translations[$mask_key]->language
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
	for my $el ($trans->elements->list) {
		my $id = $el->id;
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
	for my $trans ($self->translations->@*) {
		for my $el ($trans->elements->list) {
			next unless $el->id eq $key;
			push @found, [$trans, $el];
		}
	}

	return @found;
}

sub run_translate ($self, $key)
{
	for my $trans ($self->translations->@*) {
		say "Translating for " . $trans->language;

		my $found = first { $_->id eq $key } $trans->elements->list;
		if ($found) {
			say "It now says: " . $found->msgstr;
			say "(Leave empty to keep it)";
		}

		my $new = $self->_prompt('Translation');

		die 'No text specified and it did not exist, aborting'
			if !length $new && !$found;

		if ($found) {
			$found->msgstr($new)
				if length $new;
		}
		else {
			$trans->add_element(
				msgid => $key,
				msgstr => $new
			);
		}
	}

	for my $trans ($self->translations->@*) {
		$trans->sync;
	}

	return;
}

sub run_rename ($self, $key_from, $key_to)
{
	my @found = $self->_find_by_id($key_from);
	my @found_to = $self->_find_by_id($key_to);

	for my $data (@found) {
		for my $data_to (@found_to) {
			die "key $key_to already exists!"
				if $data->[0] eq $data_to->[0];
		}
	}

	if (@found == 0) {
		$self->_print_name_suggestions($self->translations->[0], $key_from, 4);
	}
	else {
		for my $data (@found) {
			my ($trans, $el) = @$data;
			$trans->add_element(
				{
					msgid => $key_to,
					msgstr => $el->msgstr,
				}
			);

			$trans->remove_element($el);
			$trans->sync;
		}
	}

	say 'Renamed elements: ' . scalar @found;

	return;
}

sub run_fix ($self)
{
	my @diffs = $self->_calculate_differences;
	for my $arr (sort { $a->[0] cmp $b->[0] } @diffs) {
		my ($id, @langs) = $arr->@*;

		say "$id - present in " . join ', ', @langs;
		say "Options:";
		say "1. Translate this string";
		say "2. Rename this string";
		say "3. Do nothing";

		my $choice = $self->_prompt('Choice');
		if ($choice eq '1') {
			$self->run_translate($id);
		}
		elsif ($choice eq '2') {
			my $to = $self->_prompt('New key');
			$self->run_rename($id, $to);
		}
		elsif ($choice eq '3') {

			# do nothing
		}
		else {
			say "Unknown option - lets start again";
			redo;
		}
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
		for my $data (sort { $a->[0]->language cmp $b->[0]->language } @found) {
			my ($trans, $el) = @$data;

			my $lang = $trans->language;
			my $str = $el->msgstr;
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
	for my $trans ($self->translations->@*) {
		for my $el ($trans->elements->list) {
			next unless $el->id =~ $re;
			$found{$el->id} = 1;
		}
	}

	for my $id (keys %found) {
		say $id;
	}

	return;
}

sub run ($self, @args)
{
	# Workaround for current bugs
	require Text::PO;
	@Text::PO::META = qw(Language Content-Type);

	my $show = undef;
	my $search = undef;
	my $translate = undef;
	my $rename = undef;
	my $to = undef;
	my $fix = 0;

	GetOptionsFromArray(
		\@args,
		'show|s=s' => \$show,
		'list|l=s' => \$search,
		'translate|t=s' => \$translate,
		'rename=s' => \$rename,
		'to=s' => \$to,
		fix => \$fix,
	);

	if ($translate) {
		$self->run_translate($translate);
	}

	elsif ($rename && $to) {
		$self->run_rename($rename, $to);
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

	else {
		# TODO: print a summary?
		say $self->help;
	}

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION translate [OPTIONS]
	Options:
		-s=[ID], --show [ID]  shows message strings for key ID
		-l=[QUERY], --list [QUERY]  Searches for given QUERY (substring of ID)
		-t=[ID], --translate [ID]  add or replace existing key
		--rename [ID] --to [ID]  rename translation to a different key
		--fix  prompts the user to fix all the missing translations

