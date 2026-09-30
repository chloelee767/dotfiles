package main

import "testing"

func TestParseRunRef(t *testing.T) {
	tests := []struct {
		in       string
		wantRepo string
		wantID   int64
		wantErr  bool
	}{
		{in: "123", wantID: 123},
		{in: "https://github.com/o/r/actions/runs/123", wantRepo: "o/r", wantID: 123},
		{in: "https://github.com/o/r/actions/runs/123/job/456", wantRepo: "o/r", wantID: 123},
		{in: "https://github.com/o/r/actions/runs/123/attempts/2", wantRepo: "o/r", wantID: 123},
		{in: "https://github.com/o/r/pull/5", wantErr: true},
		{in: "abc", wantErr: true},
	}
	for _, tt := range tests {
		repo, id, err := parseRunRef(tt.in)
		if (err != nil) != tt.wantErr || repo != tt.wantRepo || id != tt.wantID {
			t.Errorf("parseRunRef(%q) = %q, %d, %v; want %q, %d, err=%v", tt.in, repo, id, err, tt.wantRepo, tt.wantID, tt.wantErr)
		}
	}
}

func TestParsePRRef(t *testing.T) {
	tests := []struct {
		in       string
		wantRepo string
		wantNum  int
		wantErr  bool
	}{
		{in: "42", wantNum: 42},
		{in: "https://github.com/o/r/pull/42", wantRepo: "o/r", wantNum: 42},
		{in: "https://github.com/o/r/pull/42/files", wantRepo: "o/r", wantNum: 42},
		{in: "https://github.com/o/r/actions/runs/1", wantErr: true},
	}
	for _, tt := range tests {
		repo, n, err := parsePRRef(tt.in)
		if (err != nil) != tt.wantErr || repo != tt.wantRepo || n != tt.wantNum {
			t.Errorf("parsePRRef(%q) = %q, %d, %v; want %q, %d, err=%v", tt.in, repo, n, err, tt.wantRepo, tt.wantNum, tt.wantErr)
		}
	}
}

func TestSameRepo(t *testing.T) {
	tests := []struct {
		remote string
		want   bool
	}{
		{"https://github.com/carousell/cs-proto.git", true},
		{"git@github.com:carousell/cs-proto.git", true},
		{"https://github.com/Carousell/CS-Proto", true},
		{"https://github.com/carousell/cs-proto-gen-go.git", false},
		{"git@github.com:other/cs-proto.git", false},
	}
	for _, tt := range tests {
		if got := sameRepo(tt.remote, "carousell/cs-proto"); got != tt.want {
			t.Errorf("sameRepo(%q) = %v, want %v", tt.remote, got, tt.want)
		}
	}
}
