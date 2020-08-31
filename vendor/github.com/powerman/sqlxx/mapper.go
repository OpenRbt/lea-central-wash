package sqlxx

import (
	"strings"
	"unicode"
)

// ToSnake maps from CamelCase to snake_case.
func ToSnake(s string) string {
	var res []string
	runes := []rune(s)
	for start := len(runes) - 1; start >= 0; start-- {
		end := start
		lower := unicode.IsLower(runes[end])
		for start > 0 && lower == unicode.IsLower(runes[start-1]) {
			start--
		}
		if start > 0 && lower {
			start--
		}
		res = append(res, string(runes[start:end+1]))
	}
	for i := range res[:len(res)/2] {
		res[i], res[len(res)-1-i] = res[len(res)-1-i], res[i]
	}
	return strings.ToLower(strings.Join(res, "_"))
}
