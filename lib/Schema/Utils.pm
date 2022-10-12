package Schema::Utils;

use Exporter qw(import);
use Exception::RecordDoesNotExist;
use Exception::SearchCriteriaTooVague;

use header;

our @EXPORT_OK = qw(
	fetch_single
	ensure_single
	fetch_all
);

sub fetch_single ($rs)
{
	my $found = $rs->next;
	Exception::RecordDoesNotExist->throw unless $found;

	return $found;
}

sub fetch_all ($rs)
{
	return [$rs->all];
}

sub ensure_single ($rs)
{
	my $found = fetch_single($rs);
	Exception::SearchCriteriaTooVague->throw if $rs->next;

	return $found;
}

