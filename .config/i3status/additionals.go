package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io/ioutil"
	"net/http"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

const (
	netStatisticsSrc = "/sys/class/net/wlp0s20u1/statistics/"
	gpuTempSrc       = "http://127.0.0.1:9101/metrics/"
	gpuTempTimeout   = 500 * time.Millisecond
)

var (
	lastRefresh    time.Time
	lastRx, lastTx int
)

// Functions readableRate, readTransmission, getRate, main derived from
// https://github.com/i3/i3status/blob/master/contrib/net-speed.sh
func readableRate(bytes int, interval time.Duration) string {
	kib := bytes >> 10
	if kib < 0 {
		return "? K"
	} else if kib > 1024 {
		mibInt := kib >> 10
		mibDec := kib % 1024 * 976 / 1000
		return fmt.Sprintf("%d.%02dM", mibInt, mibDec)
	} else {
		return fmt.Sprintf("%dK", kib)
	}
}

func readTransmission(transmissionType string) (int, error) {
	filename := filepath.Join(netStatisticsSrc, transmissionType)
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		return 0, err
	}
	return strconv.Atoi(strings.TrimSpace(string(data)))
}

func getRate() (string, error) {
	time := time.Now()
	rx, err := readTransmission("rx_bytes")
	if err != nil {
		return "", err
	}
	tx, err := readTransmission("tx_bytes")
	if err != nil {
		return "", err
	}

	interval := time.Sub(lastRefresh)
	var rate string
	if interval > 0 {
		up := readableRate(tx-lastTx, interval)
		down := readableRate(rx-lastRx, interval)
		rate = fmt.Sprintf(" %s |  %s", up, down)
	} else {
		rate = ""
	}

	lastRefresh = time
	lastRx = rx
	lastTx = tx
	return rate, nil
}

func getMetrics(fullMetrics string, soughtMetrics ...string) ([]int, error) {
	metrics := make([]int, len(soughtMetrics))
	for i, metric := range soughtMetrics {
		for _, line := range strings.Split(fullMetrics, "\n") {
			if strings.HasPrefix(line, metric) {
				value, err := strconv.Atoi(line[strings.LastIndex(line, " ")+1:])
				if err != nil {
					return nil, err
				}
				metrics[i] = value
			}
		}
	}
	return metrics, nil
}

func getLoad() (string, error) {
	client := &http.Client{
		Timeout: gpuTempTimeout,
	}
	resp, err := client.Get(gpuTempSrc)
	if err != nil {
		// not available, ignore
		return "", nil
	}

	buf := new(bytes.Buffer)
	buf.ReadFrom(resp.Body)
	fullMetrics := buf.String()
	resp.Body.Close()

	metrics, err := getMetrics(fullMetrics, "utilization_gpu", "temperature_gpu", "utilization_memory")
	if err != nil {
		return "", err
	}

	gpuUtil := metrics[0]
	gpuTemp := metrics[1]
	memory := metrics[2]
	return fmt.Sprintf("  %02d%% |  %02d°C |  %02d%% |", gpuUtil, gpuTemp, memory), nil
}

func main() {
	lastRefresh = time.Now()
	lastRx = 0
	lastTx = 0

	cmd := exec.Command("i3status")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		panic(err)
	}
	cmd.Start()

	buf := bufio.NewReader(stdout)
	for {
		line, isPrefix, err := buf.ReadLine()
		if isPrefix {
			panic("i3status line overflowed buffer")
		} else if err != nil {
			panic(err)
		}
		rate, err := getRate()
		if err != nil {
			panic(err)
		}
		load, err := getLoad()
		if err != nil {
			panic(err)
		}
		fmt.Printf("%s |%s %s\n", rate, load, line)
	}
}
