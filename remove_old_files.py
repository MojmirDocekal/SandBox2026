import os
import time
import argparse
import subprocess
from typing import Iterable, Literal, Optional

AgeSource = Literal["mtime", "atime", "ctime", "git"]


def git_last_commit_timestamp(path: str) -> Optional[int]:
    """Return last commit UNIX timestamp for a tracked file using git; None if unavailable."""
    try:
        # Use -- to separate path; works even for paths starting with '-'
        result = subprocess.run(
            ["git", "log", "-1", "--format=%ct", "--", path],
            check=False,
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            return None
        value = result.stdout.strip()
        if not value:
            return None
        return int(value)
    except Exception:
        return None


def get_timestamp(path: str, source: AgeSource) -> float:
    if source == "mtime":
        return os.path.getmtime(path)
    if source == "atime":
        return os.path.getatime(path)
    if source == "ctime":
        return os.path.getctime(path)
    if source == "git":
        ts = git_last_commit_timestamp(path)
        if ts is not None:
            return float(ts)
        # fall back to filesystem mtime for untracked files
        return os.path.getmtime(path)
    # default safeguard
    return os.path.getmtime(path)


def iter_files(directory: str, recursive: bool = False, include_hidden: bool = False) -> Iterable[str]:
    def is_hidden(p: str) -> bool:
        return any(part.startswith('.') for part in os.path.relpath(p, directory).split(os.sep))

    if not recursive:
        for name in os.listdir(directory):
            p = os.path.join(directory, name)
            if os.path.isfile(p):
                if not include_hidden and is_hidden(p):
                    continue
                yield p
    else:
        for root, dirs, files in os.walk(directory):
            if not include_hidden:
                # prune hidden directories (including .git, .github, etc.)
                dirs[:] = [d for d in dirs if not d.startswith('.')]
            for name in files:
                p = os.path.join(root, name)
                if not include_hidden and is_hidden(p):
                    continue
                yield p


def remove_old_files(
    directory: str,
    exclude_filenames: Iterable[str],
    days: int = 30,
    *,
    dry_run: bool = False,
    verbose: bool = False,
    recursive: bool = False,
    include_hidden: bool = False,
    age_source: AgeSource = "mtime",
):
    now = time.time()
    cutoff = now - days * 86400

    excluded = set(os.path.normpath(os.path.join(directory, f)) for f in exclude_filenames)

    removed = 0
    considered = 0
    skipped_excluded = 0

    for file_path in iter_files(directory, recursive=recursive, include_hidden=include_hidden):
        considered += 1
        if os.path.normpath(file_path) in excluded:
            skipped_excluded += 1
            if verbose:
                print(f"Skip excluded: {file_path}")
            continue

        try:
            file_ts = get_timestamp(file_path, age_source)
        except FileNotFoundError:
            # File might have been removed concurrently
            if verbose:
                print(f"Missing (skipped): {file_path}")
            continue

        if verbose:
            print(
                f"Check: {file_path} | ts={time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(file_ts))} "
                f"| cutoff={time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(cutoff))} | source={age_source}"
            )

        if file_ts < cutoff:
            if dry_run:
                print(f"Would remove: {file_path}")
                removed += 1
            else:
                try:
                    os.remove(file_path)
                    print(f"Removed: {file_path}")
                    removed += 1
                except Exception as e:
                    print(f"Could not remove {file_path}: {e}")

    print(
        f"Done. Considered={considered}, Removed{' (or would remove)' if dry_run else ''}={removed}, "
        f"Excluded={skipped_excluded}, Source={age_source}, Days={days}, Recursive={recursive}"
    )


def main():
    parser = argparse.ArgumentParser(description="Remove files older than N days.")
    parser.add_argument(
        "directory",
        nargs="?",
        default=os.path.dirname(os.path.abspath(__file__)),
        help="Directory to clean (default: script directory)",
    )
    parser.add_argument("--days", type=int, default=30, help="Age threshold in days (default: 30)")
    parser.add_argument("--dry-run", action="store_true", help="Do not delete, just report")
    parser.add_argument("--verbose", action="store_true", help="Verbose logging of decisions")
    parser.add_argument("--recursive", action="store_true", help="Recurse into subdirectories")
    parser.add_argument(
        "--include-hidden",
        action="store_true",
        help="Include hidden files/directories (like .git, .github)",
    )
    parser.add_argument(
        "--age-source",
        choices=["mtime", "atime", "ctime", "git"],
        default="mtime",
        help="Which timestamp to use: filesystem mtime/atime/ctime or git last commit time",
    )
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Filename to exclude (can be passed multiple times)",
    )

    args = parser.parse_args()

    # Default exclusions: this script file and README.md in the target directory
    script_name = os.path.basename(__file__)
    defaults = [script_name, "README.md"]
    exclude_files = defaults + args.exclude

    remove_old_files(
        args.directory,
        exclude_files,
        days=args.days,
        dry_run=args.dry_run,
        verbose=args.verbose,
        recursive=args.recursive,
        include_hidden=args.include_hidden,
        age_source=args.age_source,
    )


if __name__ == "__main__":
    main()
